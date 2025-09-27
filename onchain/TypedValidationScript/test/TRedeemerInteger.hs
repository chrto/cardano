-- run this test with command:
-- cabal test --test-options=" redeemerInteger-test
-- or
-- cabal test --test-options="--quickcheck-tests 10000" redeemerInteger-test
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import qualified RedeemerInteger as OnChain
import           Prelude (IO, Integer, Num((-), (*)), fromIntegral)

import           Plutus.V1.Ledger.Value (valueOf)
import           Plutus.V2.Ledger.Api (Value, PubKeyHash, TxOutRef, TxOut(txOutValue)
                                     , adaSymbol, adaToken)
import           Plutus.Model (Run, mustFail, testNoErrors, adaValue
                             , defaultBabbage, TypedValidator(TypedValidator)
                             , toV2, UserSpend, Tx, DatumMode(HashDatum)
                             , userSpend, payToScript, payToKey, spendScript
                             , newUser, spend, submitTx, utxoAt, valueAt
                             , logError, Mock, runMock, initMock)
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck (Testable(property), Property, (==>), collect
                                , Arbitrary(arbitrary), Gen, choose)
import           Test.QuickCheck.Monadic (PropertyM, monadic, run, assert)
import           Data.Function (($), (.))
import           Data.Monoid ((<>))
import           Data.Foldable (Foldable(foldl))
import           Data.Functor ((<&>))
import           Data.String (String)
import           Data.Tuple (snd, fst)
import           Data.Eq ((==), Eq((/=)))
import           Data.Bool ((&&), Bool(..))
import           Control.Monad (replicateM, Monad((>>=)), mapM, unless, replicateM_)
import Data.List (replicate)

initialMockBalance :: Integer
initialMockBalance = 10000000

initialUserBalance :: Integer
initialUserBalance = 1000

main :: IO ()
main = defaultMain
  $ do
    testGroup
      "RedeemerUntyped Validator"
      [ testGroup
          "UnitTest "
          [ testGroup
              "HappyPath"
              [ happyPath "User 1 locks and user 2 takes with R = 42." $ runUnitTest 1 42 $ adaValue 150
              , happyPath "User 1 locks 3 transactions and user 2 takes all of them with R = 42." $ runUnitTest 3 42 $ adaValue 150
              ]
          , testGroup
              "ErrorPath"
              [ errorPath "User 1 locks and user 2 do not takes with R != 41." $ runUnitTest 1 41 $ adaValue 200]
          ]
      , testGroup
          "PropertyTest"
          [ testGroup
              "HappyPath"
              [ testProperty "Anything with 42 redeemer has to pass." $ prop_42Redeemer_pass False]
          , testGroup
              "ErrorPath"
              [ testProperty "Anything but 42 redeemer has to fail." $ prop_WrongRedeemer_fails False]
          ]
      ]
  where
    errorPath :: String -> Run a -> TestTree
    errorPath msg = happyPath msg . mustFail

    happyPath :: String -> Run a -> TestTree
    happyPath = testNoErrors (adaValue initialMockBalance) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------ TypeClass Instances ------------------------------------------

-- | Make Run an instance of Testable so we can use it with QuickCheck
instance Testable a => Testable (Run a) where
  property :: Testable a => Run a -> Property
  property rp = property a
    where
      a = fst $ runMock rp mock

      mock :: Mock
      mock = initMock defaultBabbage $ adaValue initialMockBalance

-- Make Value an instance of Arbitrary so QuickCheck can generate random values to test
instance Arbitrary Value where
  arbitrary :: Gen Value
  arbitrary = choose (1, initialUserBalance) <&> adaValue

---------------------------------------------------------------------------------------------------
------------------------------------------- DATA --------------------------------------------------
validatorScript :: TypedValidator () Integer
validatorScript = TypedValidator $ toV2 OnChain.validator

---------------------------------------------------------------------------------------------------
-------------------------------------- HELPER FUNCIONS---------------------------------------------
lockingTransaction :: UserSpend -> Value -> Tx
lockingTransaction utxosWithChange value = spendUtxos <> txOutput
  where
    spendUtxos :: Tx  -- create txInputs and change output
    spendUtxos = userSpend utxosWithChange

    txOutput :: Tx -- create script output
    txOutput = payToScript validatorScript buildDatum value
      where
        buildDatum :: DatumMode ()
        buildDatum = HashDatum ()

consumingTransaction :: Integer -> PubKeyHash -> [(TxOutRef, TxOut)] -> Tx
consumingTransaction redeemer pkh scriptRefUTxOs =
  foldl (<>) spendToPkh spendValidatorUtxos
  where
    spendToPkh :: Tx
    spendToPkh = payToKey pkh $ getValue scriptRefUTxOs
      where
        getValue :: [(TxOutRef, TxOut)] -> Value
        getValue utxos = foldl (<>) (adaValue 0) $ utxos <&> snd <&> txOutValue

    spendValidatorUtxos :: [Tx]
    spendValidatorUtxos = scriptRefUTxOs <&> fst <&> spendValidatorUtxo
      where
        spendValidatorUtxo :: TxOutRef -> Tx
        spendValidatorUtxo txOutRef = spendScript validatorScript txOutRef redeemer ()

---------------------------------------------------------------------------------------------------
--------------------------------------- TEST FUNCIONS----------------------------------------------
-- Function to test if both creating and consuming script UTxOs works properly
testScript :: Bool -> Integer -> Integer -> Value -> Run Bool
testScript shouldPass cnt redeemer value = do
  let initialBalance = adaValue initialUserBalance
  [pkh_1, pkh_2] <- replicateM 2 $ newUser initialBalance

  replicateM_ (fromIntegral cnt) $ spend pkh_1 value    -- get inputs and change output for locking tx
    <&> (\sp -> lockingTransaction sp value)            -- build locking transaction
    >>= submitTx pkh_1                                  -- submit transaction

  utxoAt validatorScript                         -- get UTxOs sitting at script address
    <&> consumingTransaction redeemer pkh_2      -- build unlocking transaction
    >>= (\tx -> case shouldPass of
      True  -> submitTx pkh_2 tx
      False -> mustFail $ submitTx pkh_2 tx
      )
    -- >>= submitTx pkh_2                           -- submit transaction

  let expectedPkh_1 = adaValue $ (-)
        (valueOf initialBalance adaSymbol adaToken)
        (cnt * valueOf value adaSymbol adaToken)
      expectedPkh_2 = if shouldPass
                      then foldl (<>) initialBalance $ replicate (fromIntegral cnt) value
                      else initialBalance

  mapM valueAt [pkh_1, pkh_2]                                       -- get final balance
    <&> (\[v1, v2] -> v1 == expectedPkh_1 && v2 == expectedPkh_2)   -- check if match expected balances

---------------------------------------------------------------------------------------------------
------------------------------------ UNIT TEST FUNCIONS--------------------------------------------
runUnitTest :: Integer -> Integer -> Value -> Run ()
runUnitTest cnt redeemer value = testScript True cnt redeemer value
    >>= (`unless` logError "Unexpected final balances")             -- log message if not expected balances

---------------------------------------------------------------------------------------------------
---------------------------------- PROPERTY TEST FUNCIONS------------------------------------------

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING PROPERTIES ------------------------------------------

prop_WrongRedeemer_fails :: Bool -> Integer -> Value -> Property
prop_WrongRedeemer_fails col redeemer value = (redeemer /= 42) ==> checkProperty col False redeemer value

prop_42Redeemer_pass :: Bool -> Value -> Property
prop_42Redeemer_pass col = checkProperty col True 42

---------------------------------------------------------------------------------------------------
------------------------------------- RUNNING THE TESTS -------------------------------------------

checkProperty :: Bool -> Bool -> Integer -> Value -> Property
checkProperty True shouldPass redeemer value = collect (redeemer, valueOf value adaSymbol adaToken) $ runChecks shouldPass redeemer value
checkProperty False shouldPass redeemer value = runChecks shouldPass redeemer value

runChecks :: Bool -> Integer -> Value -> Property
runChecks shouldPass redeemer value = monadic property check
  where
    check :: PropertyM Run ()
    check = run (testScript shouldPass 1 redeemer value)
      >>= assert
