{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module VestingParametrizedTwo where
import Plutus.V2.Ledger.Api       (Validator, PubKeyHash, POSIXTime, ScriptContext (scriptContextTxInfo), TxInfo (txInfoValidRange), from, mkValidatorScript)
import PlutusTx                   (BuiltinData, compile, liftCode, applyCode)
import PlutusTx.Prelude           (traceIfFalse, Bool, (&&), ($))
import Plutus.V2.Ledger.Contexts  (txSignedBy)
import Plutus.V1.Ledger.Interval  (contains)
import Utils (wrapValidator)


{-# INLINABLE mkVestingValidator #-}

mkVestingValidator :: PubKeyHash -> POSIXTime -> () -> () -> ScriptContext -> Bool
mkVestingValidator pkh deadline () () ctx =
  traceIfFalse "beneficiary's signature missing" signedByBeneficiary
  && traceIfFalse "deadline not reached" deadlineReached
    where
      txInfo :: TxInfo
      txInfo = scriptContextTxInfo ctx

      signedByBeneficiary :: Bool
      signedByBeneficiary = txSignedBy txInfo pkh

      deadlineReached :: Bool
      deadlineReached = contains (from deadline ) $ txInfoValidRange txInfo

wrappedMkVestingValidator :: PubKeyHash -> POSIXTime -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedMkVestingValidator pkh deadline = wrapValidator $ mkVestingValidator pkh deadline

validator :: PubKeyHash -> POSIXTime -> Validator
validator pkh deadline = mkValidatorScript ($$(compile [||wrappedMkVestingValidator||]) `applyCode` liftCode pkh `applyCode` liftCode deadline)
