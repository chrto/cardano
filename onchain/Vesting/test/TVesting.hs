-- run this test with command:
-- 'cabal test --test-options=" vesting-test'
-- or
-- 'cabal test --test-options="--quickcheck-tests 10000" vesting-test' to run 10000 test
-- or
-- 'cabal test --test-options="--quickcheck-tests 10 --use-collect-mode True" vesting-test' to print generated values

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import qualified  Vesting as OnChain

import            Prelude (IO, Integer, fromIntegral, Num ((-), (*)), Int)

import            Plutus.V2.Ledger.Api (Value, POSIXTime (POSIXTime, getPOSIXTime), PubKeyHash)
import            Plutus.V2.Ledger.Tx (TxOutRef, TxOut (txOutValue))
import            Plutus.V1.Ledger.Value (valueOf)

import            Plutus.Model.Mock (Run, utxoAt, logError, Mock, runMock, initMock)
import            Plutus.Model.Mock.MockConfig (defaultBabbage)
import            Plutus.Model.Ada (adaValue, adaSymbol, adaToken)
import            Plutus.Model.Contract (waitUntil, mustFail, testNoErrors
                                      , UserSpend, Tx, DatumMode(HashDatum)
                                      , userSpend, payToScript, payToKey
                                      , spendScript, newUser, spend, submitTx
                                      , valueAt, currentTimeRad, validateIn)
import            Plutus.Model.Validator (TypedValidator (TypedValidator), toV2)

import            Test.Tasty (defaultMainWithIngredients, testGroup, defaultIngredients, askOption)
import            Test.Tasty.Runners (TestTree)
import            Test.Tasty.Options (IsOption (defaultValue, parseValue, optionHelp, optionName), safeRead, OptionDescription (Option))
import            Test.Tasty.Ingredients (Ingredient)
import            Test.Tasty.QuickCheck (testProperty)

import            Test.QuickCheck.Property (Testable (property), Property, (==>), collect)
import            Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))
import            Test.QuickCheck.Gen (Gen, choose)
import            Test.QuickCheck.Monadic (PropertyM, monadic, run, assert)
import            Test.Tasty.Ingredients.Basic(includingOptions)

import            Data.String (String)
import            Data.Function (($), (.))
import            Data.Monoid ((<>))
import            Data.Maybe (Maybe)
import            Data.List (foldl, replicate)
import            Data.Functor ((<&>), (<$>))
import            Data.Tuple (snd, fst, uncurry)
import            Data.Bool (Bool (..), (&&))
import            Data.Eq ((==), Eq)
import            Data.Ord ((<=), (>), Ord)
import            Data.Data (Typeable, Proxy (Proxy))

import            Control.Monad (replicateM, replicateM_, Monad ((>>=), return), mapM, unless)
import            Control.Applicative ((<*>))

newtype UseCollectMode = UseCollectMode Bool
  deriving (Eq, Ord, Typeable)

instance IsOption UseCollectMode where
  defaultValue :: UseCollectMode
  defaultValue = UseCollectMode False

  parseValue :: String -> Maybe UseCollectMode
  parseValue val = UseCollectMode <$> safeRead val
  -- parseValue = (<$>) UseCollectMode . safeRead

  optionName = return "use-collect-mode"
  optionHelp = return "Run tests in collect mode (boolean). Show generated values by QickCheck."

main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where
    errorPath :: String -> Run a -> TestTree
    errorPath msg = happyPath msg . mustFail

    happyPath :: String -> Run a -> TestTree
    happyPath = testNoErrors (adaValue initialMockBalance) defaultBabbage

    ingredients :: [Ingredient]
    ingredients = includingOptions [Option (Proxy :: Proxy UseCollectMode)]:defaultIngredients

    tests :: TestTree
    tests = askOption $ \(UseCollectMode collectMode) ->
      testGroup
        "Vesting Validator"
        [ testGroup
            "UnitTest"
            [ testGroup
                "HappyPath"
                [ happyPath "Sender locks 100 Ada into Vesting contract and beneficiary should take them after deadline." $ initUsers 2 >>= (\[sender,claimer] -> runUnitTest 1 (OnChain.VestingDatum claimer 500) sender claimer 100)
                , happyPath "Sender locks 3x100 Ada into Vesting contract and beneficiary should take them all after deadline." $ initUsers 2 >>= (\[sender,claimer] -> runUnitTest 3 (OnChain.VestingDatum claimer 500) sender claimer 100)
                ]
            , testGroup
                "ErrorPath"
                [ errorPath "Sender locks 100 Ada into Vesting contract and beneficiary should not take them before deadline." $ initUsers 2 >>= (\[sender,claimer] -> runUnitTest 1 (OnChain.VestingDatum claimer 5000) sender claimer 100)
                , errorPath "Sender locks 100 Ada into Vesting contract and claimer should not take them, if not beneficiary." $ initUsers 3 >>= (\[sender,claimer,beneficiary] -> runUnitTest 1 (OnChain.VestingDatum beneficiary 500) sender claimer 100)
                ]
            ]
        , testGroup
            "PropertyTest"
            [ testGroup
                "HappyPath"
                [ testProperty "Beneficiary can claim after deadline." $ prop_AfterDeadline_pass collectMode ]
            , testGroup
                "ErrorPath"
                [ testProperty "Beneficiary can not claim before deadline." $ prop_BeforeDeadline_fails collectMode ]
            ]
        ]

---------------------------------------------------------------------------------------------------
--------------------------------- PROPERTY TEST INSTANCE ------------------------------------------
-- | Make Run an instance of Testable so we can use it with QuickCheck
instance Testable a => Testable (Run a) where
  property :: Testable a => Run a -> Property
  property rp = property a
    where
      a = fst $ runMock rp mock

      mock :: Mock
      mock = initMock defaultBabbage $ adaValue initialMockBalance

-- Make POSIXTime an instance of Arbitrary so QuickCheck can generate random values to test
instance Arbitrary POSIXTime where
  arbitrary :: Gen POSIXTime
  arbitrary = choose (0, 2000) <&> POSIXTime
---------------------------------------------------------------------------------------------------
------------------------------------------- DATA --------------------------------------------------
validatorScript :: TypedValidator OnChain.VestingDatum ()
validatorScript = TypedValidator $ toV2 OnChain.validator

consumeTxAfter :: POSIXTime
consumeTxAfter = POSIXTime 1000

initialMockBalance :: Integer
initialMockBalance = 10000000

initialUserBalance :: Value
initialUserBalance = adaValue 1000

initUsers :: Int -> Run [PubKeyHash]
initUsers cnt = replicateM cnt $ newUser initialUserBalance
---------------------------------------------------------------------------------------------------
-------------------------------------- HELPER FUNCIONS---------------------------------------------
lockingTransaction :: OnChain.VestingDatum -> UserSpend -> Value -> Tx
lockingTransaction datum utxosWithChange value = spendUtxos <> txOutput
  where
    spendUtxos :: Tx  -- create txInputs and change output
    spendUtxos = userSpend utxosWithChange

    txOutput :: Tx -- create script output
    txOutput = payToScript validatorScript buildDatum value
      where
        buildDatum :: DatumMode OnChain.VestingDatum
        buildDatum = HashDatum datum

consumingTransaction :: OnChain.VestingDatum -> [(TxOutRef, TxOut)] -> Tx
consumingTransaction datum scriptRefUTxOs =
  foldl (<>) spendToPkh spendValidatorUtxos
  where
    spendToPkh :: Tx
    spendToPkh = payToKey getPkh $ getValue scriptRefUTxOs
      where
        getValue :: [(TxOutRef, TxOut)] -> Value
        getValue utxos = foldl (<>) (adaValue 0) $ utxos <&> snd <&> txOutValue

        getPkh :: PubKeyHash
        getPkh = OnChain.beneficiary datum

    spendValidatorUtxos :: [Tx]
    spendValidatorUtxos = scriptRefUTxOs <&> fst <&> spendValidatorUtxo
      where
        spendValidatorUtxo :: TxOutRef -> Tx
        spendValidatorUtxo txOutRef = spendScript validatorScript txOutRef () datum

---------------------------------------------------------------------------------------------------
--------------------------------------- TEST FUNCIONS----------------------------------------------
-- Function to test if both creating and consuming script UTxOs works properly
testScript :: Bool -> Integer -> OnChain.VestingDatum -> PubKeyHash -> PubKeyHash -> Value -> Run Bool
testScript shouldPass cnt datum sender claimer value = do

  replicateM_ (fromIntegral cnt)
    $ spend sender value                          -- get inputs and change output for locking tx
    <&> (\sp -> lockingTransaction datum sp value)     -- build locking transaction
    >>= submitTx sender                           -- submit transaction

  waitUntil consumeTxAfter            -- wait 1000 ms

  (currentTimeRad 100 <&> (,))            -- create tx valid range
    <*> (utxoAt validatorScript          -- get UTxOs sitting at script address
    <&> consumingTransaction datum)      -- build unlocking transaction
    >>= uncurry validateIn               -- set valid range into tx
    >>= (\tx -> case shouldPass of               -- submit transaction
           True  -> submitTx claimer tx
           False -> mustFail $ submitTx claimer tx)


  let expectedSender = adaValue
        $ (-)
          (valueOf initialUserBalance adaSymbol adaToken)
          (cnt * valueOf value adaSymbol adaToken)
      expectedClaimer =
        if shouldPass
        then foldl (<>) initialUserBalance $ replicate (fromIntegral cnt) value
        else initialUserBalance

  mapM
    valueAt
    [sender, claimer]                                                  -- get final balance
    <&> (\[v1, v2] -> v1 == expectedSender && v2 == expectedClaimer)   -- check if match expected balances
---------------------------------------------------------------------------------------------------
------------------------------------ UNIT TEST FUNCIONS--------------------------------------------
runUnitTest :: Integer -> OnChain.VestingDatum -> PubKeyHash -> PubKeyHash -> Integer -> Run ()
runUnitTest cnt datum sender claimer value = testScript True cnt datum sender claimer (adaValue value)
    >>= (`unless` logError "Unexpected final balances")             -- log message if not expected balances

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING PROPERTIES ------------------------------------------
prop_AfterDeadline_pass :: Bool -> POSIXTime -> Property
prop_AfterDeadline_pass col deadline = (deadline <= consumeTxAfter) ==> checkProperty col True deadline

prop_BeforeDeadline_fails :: Bool -> POSIXTime -> Property
prop_BeforeDeadline_fails col deadline = (deadline > consumeTxAfter) ==> checkProperty col False deadline

---------------------------------------------------------------------------------------------------
------------------------------------- RUNNING THE TESTS -------------------------------------------

checkProperty :: Bool -> Bool -> POSIXTime -> Property
checkProperty True shouldPass deadline = collect (getPOSIXTime deadline) $ runChecks shouldPass deadline
checkProperty False shouldPass deadline = runChecks shouldPass deadline

runChecks :: Bool -> POSIXTime -> Property
runChecks shouldPass deadline = monadic property check
  where
    check :: PropertyM Run ()
    check = run (initUsers 2 >>= (\[sender, claimer] -> testScript shouldPass 1 (OnChain.VestingDatum claimer deadline) sender claimer (adaValue 100)))
      >>= assert
