{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import qualified NegativeRTimed as OnChain
import           Test.Tasty (defaultMain, testGroup, TestTree)
import           Prelude (IO, String, (<>))
import           Plutus.Model (mustFail, testNoErrors, adaValue, defaultBabbage
                             , Run, newUser, TypedValidator(TypedValidator)
                             , toV2, Tx, UserSpend, userSpend, payToScript
                             , DatumMode(HashDatum), spendScript, payToKey, spend, submitTx, waitUntil, utxoAt, currentTimeRad, validateIn, valueAt, logError)
import           PlutusTx.Prelude ((.), ($), Integer, Eq ((==)), (&&) )
import           Plutus.V2.Ledger.Api (POSIXTime, PubKeyHash, Value, TxOutRef, TxOut (txOutValue))
import           Control.Monad (replicateM, mapM, unless, (>>=))
import           Data.Functor ((<&>))

main :: IO ()
main = defaultMain
  $ do
    testGroup
      "UnitTest Validator"
      [
        testGroup
          "HappyPath"
          [
            testGroup "Negative Redeemer and After Deadline" [
              happyPath "User 1 locks and user 2 takes with R = -42 after dealine succeeds" $ testScript 50 (-42)
            ]
            , testGroup "Redeemer is 0 and After Deadline" [
            happyPath "User 1 locks and user 2 takes with R = 0   after dealine succeeds" $ testScript 50 0
            ]
          ]
        , testGroup
          "ErrorPath"
          [
            testGroup "Positive Redeemeer and After Deadline" [
              errorPath  "User 1 locks and user 2 takes with R = 42  after dealine fails   " $ testScript 50 42
            ]
            , testGroup "Negative Redeemer and Before Deadline" [
              errorPath  "User 1 locks and user 2 takes with R = -42 before dealine fails  " $ testScript 5000 (-42)
            ]
            , testGroup "Redeemer is 0 Before Deadline" [
              errorPath  "User 1 locks and user 2 takes with R = 0   before dealine fails  " $ testScript 5000 0
            ]
            , testGroup "Positive Redeemer Before Deadline" [
              errorPath  "User 1 locks and user 2 takes with R = 42  before dealine fails  " $ testScript 5000 42
            ]
          ]
      ]
  where
    errorPath :: String -> Run a -> TestTree
    errorPath msg = happyPath msg . mustFail

    happyPath :: String -> Run a -> TestTree
    happyPath = testNoErrors (adaValue 10000000) defaultBabbage

consumeTxAfter :: POSIXTime
consumeTxAfter = 1000

validatorScript :: TypedValidator OnChain.DeadlineDatum Integer
validatorScript = TypedValidator $ toV2 OnChain.validator

-- Create transaction that spends utxos and lock Lovelace in validatorScript
-- The 'UserSpend' is a special structure that alongsied with spending UTXO also contains the exchange UTXOs
-- that user will pay back to ensure that all UTXOs are fully spent.
-- To spend those UTxO you we have to call 'userSpend'.
lockingTransaction :: POSIXTime -> UserSpend -> Value -> Tx
lockingTransaction
  deadline
  utxosWithChange
  value = spendUtxos <> txOutput -- concatinate into tx with all inputs and outputs
  where
    spendUtxos :: Tx  -- create txInputs and change output
    spendUtxos = userSpend utxosWithChange

    txOutput :: Tx -- create script output
    txOutput = payToScript validatorScript buildDatum value
      where
        buildDatum :: DatumMode OnChain.DeadlineDatum
        buildDatum = HashDatum $ OnChain.MkDeadlineDatum deadline

consumingTransaction :: POSIXTime -> Integer -> PubKeyHash -> TxOutRef -> Value -> Tx
consumingTransaction deadline redeemer pkh scriptRefUTxO value = spendValidatorUtxo <> spendToPkh
  where
    buildDataum :: OnChain.DeadlineDatum
    buildDataum = OnChain.MkDeadlineDatum deadline

    spendValidatorUtxo :: Tx
    spendValidatorUtxo = spendScript
      validatorScript
      scriptRefUTxO
      redeemer
      buildDataum

    spendToPkh :: Tx
    spendToPkh = payToKey pkh value

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING REDEEMERS -------------------------------------------

-- Function to test if both creating and consuming script UTxOs works properly
testScript :: POSIXTime -> Integer -> Run ()
testScript deadline redeemer = do
  [pkh_1, pkh_2] <- replicateM 2 $ newUser $ adaValue 1000

  let value = adaValue 100
  spend pkh_1 value                                       -- get inputs and change output for locking tx
    <&> (\sp -> lockingTransaction deadline sp value)     -- create locking transaction
    >>= submitTx pkh_1                                    -- submit transaction

  waitUntil consumeTxAfter            -- wait 1000 ms

  utxos <- utxoAt validatorScript     -- get all utxos sitting at script address
  let [(ref, out)] = utxos            -- there is only one utxo at script address
      claimTx = consumingTransaction deadline redeemer pkh_2 ref (txOutValue out)
  currentTimeRad 100              -- Create time interval with equal radius around current time (it creates interval of [currentTime - rad, currentTime + rad].)
    >>= (`validateIn` claimTx)    -- Sed valid tx range into script consuming transaction
    >>= submitTx pkh_2            -- Submit script consuming transaction

  mapM valueAt [pkh_1, pkh_2]                                       -- get final balances
    <&> (\[v1, v2] -> v1 == adaValue 900 && v2 == adaValue 1100)    -- check if match expected balances
    >>= (`unless` logError "Unexpected final balances")             -- log message if not expected balances