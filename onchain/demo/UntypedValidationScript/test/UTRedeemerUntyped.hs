{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import qualified RedeemerUntyped as OnChain
import           Prelude (IO, Integer, Num ((-)))

import           Plutus.V2.Ledger.Api (BuiltinData, Value, ToData (toBuiltinData), PubKeyHash, TxOutRef, TxOut (txOutValue), adaSymbol, adaToken)
import           Plutus.V1.Ledger.Value (valueOf)

import           Plutus.Model (Run, mustFail, testNoErrors, adaValue
                             , defaultBabbage, TypedValidator (TypedValidator), toV2, UserSpend, Tx, DatumMode (HashDatum), userSpend, payToScript, payToKey, spendScript, newUser, spend, submitTx, utxoAt, valueAt, logError)
import           Test.Tasty (TestTree, defaultMain, testGroup)

import           Data.Function (($), (.))
import           Data.Monoid ((<>))
import           Data.Foldable (Foldable(foldl))
import           Data.Functor ((<&>))
import           Data.String (String)
import           Data.Tuple (snd, fst)
import           Data.Eq ((==))
import           Data.Bool ((&&))

import           Control.Monad (replicateM, Monad ((>>=)), mapM, unless)

main :: IO ()
main = defaultMain
  $ do
    testGroup
      "UnitTest RedeemerUntyped Validator"
      [testGroup
        "HappyPath"
        [happyPath "User 1 locks and user 2 takes with R = 42." $ testScript (adaValue 150) 42]
      , testGroup
        "ErrorPath"
        [errorPath "User 1 locks and user 2 do not takes with R != 41." $ testScript (adaValue 200) 41]
      ]
  where
    errorPath :: String -> Run a -> TestTree
    errorPath msg = happyPath msg . mustFail

    happyPath :: String -> Run a -> TestTree
    happyPath = testNoErrors (adaValue 10000000) defaultBabbage

---------------------------------------------------------------------------------------------------
------------------------------------------- DATA --------------------------------------------------
validatorScript :: TypedValidator BuiltinData BuiltinData
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
        buildDatum :: DatumMode BuiltinData
        buildDatum = HashDatum (toBuiltinData ())

consumingTransaction :: Integer -> PubKeyHash -> [(TxOutRef, TxOut)] -> Tx
consumingTransaction redeemer pkh scriptRefUTxOs =
  foldl (<>) spendToPkh  spendValidatorUtxos
  where
    spendToPkh :: Tx
    spendToPkh = payToKey pkh $ getValue scriptRefUTxOs
      where
        getValue :: [(TxOutRef, TxOut)] -> Value
        getValue utxos = foldl (<>) (adaValue 0) $ utxos <&> snd <&> txOutValue

    spendValidatorUtxos ::[Tx]
    spendValidatorUtxos = scriptRefUTxOs <&> fst <&> spendValidatorUtxo
      where
        spendValidatorUtxo :: TxOutRef -> Tx
        spendValidatorUtxo txOutRef = let buildRedeemer = toBuiltinData redeemer
                                          buildDatum = toBuiltinData ()
                                      in  spendScript validatorScript txOutRef buildRedeemer buildDatum

---------------------------------------------------------------------------------------------------
--------------------------------------- TEST FUNCIONS----------------------------------------------
-- Function to test if both creating and consuming script UTxOs works properly
testScript :: Value -> Integer -> Run ()
testScript value redeemer = do
  let initialBalance  = adaValue 1000
  [pkh_1, pkh_2] <- replicateM 2 $ newUser initialBalance

  spend pkh_1 value                              -- get inputs and change output for locking tx
    <&> (\sp -> lockingTransaction sp value)     -- build locking transaction
    >>= submitTx pkh_1                           -- submit transaction

  utxoAt validatorScript                         -- get UTxOs sitting at script address
    <&> consumingTransaction redeemer pkh_2      -- build unlocking transaction
    >>= submitTx pkh_2                           -- submit transaction

  let expectedPkh_1 = adaValue $ (-) (valueOf initialBalance adaSymbol adaToken) (valueOf value adaSymbol adaToken)
      expectedPkh_2 = initialBalance <> value
  mapM valueAt [pkh_1, pkh_2]                                     -- get final balance
    <&> (\[v1, v2] -> v1 == expectedPkh_1 && v2 == expectedPkh_2)  -- check if match expected balances
    >>= (`unless` logError "Unexpected final balances")           -- log message if not expected balances
