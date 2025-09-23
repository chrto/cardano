{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module NegativeRTimed where

import           Plutus.V2.Ledger.Api (POSIXTime
                                     , ScriptContext(scriptContextTxInfo)
                                     , TxInfo(txInfoValidRange), BuiltinData
                                     , Validator, mkValidatorScript
                                     , POSIXTimeRange)
import           PlutusTx (unstableMakeIsData, compile)
import           PlutusTx.Prelude (traceIfFalse, Bool, (&&), Integer, Ord((<=))
                                 , ($))
import           Plutus.V1.Ledger.Interval (contains, from)
import           Utils (wrapValidator)

newtype DeadlineDatum = MkDeadlineDatum { deadline :: POSIXTime }
unstableMakeIsData ''DeadlineDatum

{-# INLINABLE mkValidator #-}
mkValidator :: DeadlineDatum -> Integer -> ScriptContext -> Bool
mkValidator datum redeemer ctx =
  traceIfFalse "Negative redeemer has been expected!" validateRedeemer
  && traceIfFalse "Deadline has not been reached!" validateDeadline
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    txValidRange :: POSIXTimeRange
    txValidRange = txInfoValidRange txInfo

    afterDeadlineRange :: POSIXTimeRange
    afterDeadlineRange = from $ deadline datum

    validateRedeemer :: Bool
    validateRedeemer = redeemer <= 0

    validateDeadline :: Bool
    validateDeadline = contains afterDeadlineRange txValidRange

wrappedMkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedMkValidator = wrapValidator mkValidator

validator :: Validator
validator = mkValidatorScript $$(compile [||wrappedMkValidator||])
