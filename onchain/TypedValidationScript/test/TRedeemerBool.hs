{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import qualified RedeemerBool as OnChain

import           Prelude (Integer, IO, fromIntegral, Num ((-), (*)))

import           Plutus.V2.Ledger.Api (Value, PubKeyHash, TxOutRef, TxOut (txOutValue), adaSymbol, adaToken)
import           Plutus.V1.Ledger.Value (valueOf)

import           Plutus.Model (Run, mustFail, testNoErrors, adaValue, defaultBabbage, TypedValidator (TypedValidator), toV2, UserSpend, Tx, DatumMode (HashDatum), userSpend, payToScript, payToKey, spendScript, newUser, spend, submitTx, utxoAt, valueAt, logError)
import           Test.Tasty (TestTree, testGroup, defaultMain)

import           Data.Bool (Bool (..), (&&))
import           Data.Eq (Eq ((==)))
import           Data.String (String)
import           Data.Function (($))
import           Data.Tuple (fst, snd)
import           Data.Functor ((<&>))
import           Data.Monoid ((<>))
import           Data.List (foldl, replicate)

import           Control.Monad (Monad((>>=)), replicateM, replicateM_, mapM, unless)
import           Control.Category ((.))

initialMockBalance :: Integer
initialMockBalance = 10000000

initialUserBalance :: Integer
initialUserBalance = 1000

main :: IO ()
main = defaultMain
  $ do
    testGroup
      "RedeemerBool Validator"
      [ testGroup
          "HappyPath"
          [ happyPath "User 1 locks and user 2 takes with redeemer (True True)." $ runUnitTest 1 (True, True) $ adaValue 150
          , happyPath "User 1 locks 3 transactions and user 2 takes all of them with (True True)." $ runUnitTest 3 (True, True) $ adaValue 150
          ]
      , testGroup
          "ErrorPath"
          [ errorPath "User 1 locks and user 2 do not takes with redeemer (False False)." $ runUnitTest 1 (False, False) $ adaValue 150
          , errorPath "User 1 locks and user 2 do not takes with redeemer (True False)." $ runUnitTest 1 (True, False) $ adaValue 150
          , errorPath "User 1 locks and user 2 do not takes with redeemer (False True)." $ runUnitTest 1 (False, True) $ adaValue 150
          ]
      ]
  where
    errorPath :: String -> Run a -> TestTree
    errorPath msg = happyPath msg . mustFail

    happyPath :: String -> Run a -> TestTree
    happyPath = testNoErrors (adaValue initialMockBalance) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------------- DATA --------------------------------------------------
validatorScript :: TypedValidator () (Bool, Bool)
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

consumingTransaction :: (Bool, Bool) -> PubKeyHash -> [(TxOutRef, TxOut)] -> Tx
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
testScript :: Bool -> Integer -> (Bool, Bool) -> Value -> Run Bool
testScript shouldPass cnt redeemer value = do
  let initialBalance = adaValue initialUserBalance
  [pkh_1, pkh_2] <- replicateM 2 $ newUser initialBalance
  replicateM_ (fromIntegral cnt)
    $ spend pkh_1 value                          -- get inputs and change output for locking tx
    <&> (\sp -> lockingTransaction sp value)     -- build locking transaction
    >>= submitTx pkh_1                           -- submit transaction

  utxoAt validatorScript                         -- get UTxOs sitting at script address
    <&> consumingTransaction redeemer pkh_2      -- build unlocking transaction
    >>= (\tx -> case shouldPass of               -- submit transaction
           True  -> submitTx pkh_2 tx
           False -> mustFail $ submitTx pkh_2 tx)

  let expectedPkh_1 = adaValue
        $ (-)
          (valueOf initialBalance adaSymbol adaToken)
          (cnt * valueOf value adaSymbol adaToken)
      expectedPkh_2 =
        if shouldPass
        then foldl (<>) initialBalance $ replicate (fromIntegral cnt) value
        else initialBalance
  mapM
    valueAt
    [pkh_1, pkh_2]                                                  -- get final balance
    <&> (\[v1, v2] -> v1 == expectedPkh_1 && v2 == expectedPkh_2)   -- check if match expected balances

---------------------------------------------------------------------------------------------------
------------------------------------ UNIT TEST FUNCIONS--------------------------------------------
runUnitTest :: Integer -> (Bool, Bool) -> Value -> Run ()
runUnitTest cnt redeemer value = testScript True cnt redeemer value
    >>= (`unless` logError "Unexpected final balances")             -- log message if not expected balances
