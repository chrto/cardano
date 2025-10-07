-- run this test with command:
-- cabal test --test-options=" negativeRTimed-property-test
-- or
-- cabal test --test-options="--quickcheck-tests 10000" negativeRTimed-property-test
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import qualified NegativeRTimed as OnChain
import           Prelude (IO, fst, ($), Semigroup((<>)), (>), Eq((==)), (&&)
                        , Bool (False, True), Ord ((<=)))
import           Plutus.Model (Run, runMock, initMock, defaultBabbage, adaValue
                             , Mock, TypedValidator(TypedValidator), UserSpend
                             , Tx, DatumMode(HashDatum), toV2, userSpend
                             , payToScript, spendScript, payToKey, newUser
                             , spend, submitTx, waitUntil, utxoAt
                             , currentTimeRad, validateIn, valueAt
                             , mustFail)
import           Plutus.V2.Ledger.Api (POSIXTime(POSIXTime, getPOSIXTime), Value, PubKeyHash
                                     , TxOutRef, TxOut(txOutValue))
import           Data.Functor ((<$>), (<&>))
import           PlutusTx.Prelude (Integer)
import           Control.Monad (replicateM, Monad((>>=)), mapM)
import           Test.Tasty (defaultMain, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck.Monadic (monadic, run, assert, PropertyM)
import           Test.QuickCheck (Testable (property), Arbitrary(arbitrary), choose, Gen
                                , Property, (==>), collect)

main :: IO ()
main = defaultMain
  $ do
    testGroup
      "PropertyTest Validator"
      [ testGroup
        "HappyPath"
        [ testProperty "Anything after deadline with negative or 0 redeemer has to pass" $ prop_NegativeRedeemerAfterDeadline_pass False]
      , testGroup
        "ErrorPath"
        [ testProperty "Anything before deadline has to fail" $ prop_BeforeDeadline_fails False
        , testProperty "Positive redeemer has to fail" $ prop_PositiveRedeemer_fails False
        ]
      ]

-- | Make Run an instance of Testable so we can use it with QuickCheck
instance Testable a => Testable (Run a) where
  property :: Testable a => Run a -> Property
  property rp = property a
    where
      a = fst $ runMock rp mock

      mock :: Mock
      mock = initMock defaultBabbage $ adaValue 10000000

-- Make POSIXTime an instance of Arbitrary so QuickCheck can generate random values to test
instance Arbitrary POSIXTime where
  arbitrary :: Gen POSIXTime
  arbitrary = POSIXTime <$> choose (0, 2000)

consumeTxAfter :: POSIXTime
consumeTxAfter = 1000

validatorScript :: TypedValidator OnChain.DeadlineDatum Integer
validatorScript = TypedValidator $ toV2 OnChain.validator

-- Create transaction that spends utxos and lock Lovelace in validatorScript
-- The 'UserSpend' is a special structure that alongsied with spending UTXO also contains the exchange UTXOs
-- that user will pay back to ensure that all UTXOs are fully spent.
-- To spend those UTxO you we have to call 'userSpend'.
lockingTransaction :: POSIXTime -> UserSpend -> Value -> Tx
lockingTransaction deadline utxosWithChange value = spendUtxos <> txOutput -- concatinate into tx with all inputs and outputs
  where
    spendUtxos :: Tx  -- create txInputs and change output
    spendUtxos = userSpend utxosWithChange

    txOutput :: Tx -- create script output
    txOutput = payToScript validatorScript buildDatum value
      where
        buildDatum :: DatumMode OnChain.DeadlineDatum
        buildDatum = HashDatum $ OnChain.MkDeadlineDatum deadline

consumingTransaction
  :: POSIXTime -> Integer -> PubKeyHash -> TxOutRef -> Value -> Tx
consumingTransaction deadline redeemer pkh scriptRefUTxO value =
  spendValidatorUtxo <> spendToPkh
  where
    buildDataum :: OnChain.DeadlineDatum
    buildDataum = OnChain.MkDeadlineDatum deadline

    spendValidatorUtxo :: Tx
    spendValidatorUtxo =
      spendScript validatorScript scriptRefUTxO redeemer buildDataum

    spendToPkh :: Tx
    spendToPkh = payToKey pkh value

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING PROPERTIES ------------------------------------------
prop_BeforeDeadline_fails :: Bool -> POSIXTime -> Integer -> Property
prop_BeforeDeadline_fails col deadline redeemer = (deadline > 1000) ==> checkProperty col False deadline redeemer

prop_PositiveRedeemer_fails :: Bool -> POSIXTime -> Integer -> Property
prop_PositiveRedeemer_fails col deadline redeemer = (redeemer > 0) ==> checkProperty col False deadline redeemer

prop_NegativeRedeemerAfterDeadline_pass :: Bool -> POSIXTime -> Integer -> Property
prop_NegativeRedeemerAfterDeadline_pass col deadline redeemer = (redeemer <= 0 && deadline <= 1000) ==> checkProperty col True deadline redeemer
---------------------------------------------------------------------------------------------------
------------------------------------- RUNNING THE TESTS -------------------------------------------

checkProperty :: Bool -> Bool -> POSIXTime -> Integer -> Property
checkProperty True shouldPass deadline redeemer = collect (redeemer, getPOSIXTime deadline) $ runChecks shouldPass deadline redeemer
checkProperty False shouldPass deadline redeemer = runChecks shouldPass deadline redeemer

runChecks :: Bool -> POSIXTime -> Integer -> Property
runChecks shouldPass deadline redeemer = monadic property check
  where
    check :: PropertyM Run ()
    check = run (testScript shouldPass deadline redeemer)
      >>= assert

-- Function to test if both creating and consuming script UTxOs works properly
testScript :: Bool -> POSIXTime -> Integer -> Run Bool
testScript shouldPass deadline redeemer = do
  [pkh_1, pkh_2] <- replicateM 2 $ newUser $ adaValue 1000

  let value = adaValue 100
  spend pkh_1 value                                       -- get inputs and change output for locking tx
    <&> (\sp -> lockingTransaction deadline sp value)     -- create locking transaction
    >>= submitTx pkh_1                                    -- submit transaction

  waitUntil consumeTxAfter            -- wait 1000 ms

  utxos <- utxoAt validatorScript     -- get all utxos sitting at script address
  let [(oRef, oOut)] = utxos            -- there is only one utxo at script address
      claimTx = consumingTransaction deadline redeemer pkh_2 oRef (txOutValue oOut)
      expectedValue = if shouldPass
                      then adaValue 1100
                      else adaValue 1000
  currentTimeRad 100              -- Create time interval with equal radius around current time (it creates interval of [currentTime - rad, currentTime + rad].)
    >>= (`validateIn` claimTx)    -- Sed valid tx range into script consuming transaction
    >>= (\tx -> if shouldPass
                then submitTx pkh_2 tx
                else mustFail $ submitTx pkh_2 tx)           -- Submit script consuming transaction

  mapM valueAt [pkh_1, pkh_2]                                       -- get final balances
    <&> (\[v1, v2] -> v1 == adaValue 900 && v2 == expectedValue)    -- check if match expected balances
