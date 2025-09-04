{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module VestingParametrizedBeneficiary where
import Plutus.V2.Ledger.Api       (Validator, PubKeyHash, POSIXTime, ScriptContext (scriptContextTxInfo), TxInfo (txInfoValidRange), from, mkValidatorScript)
import PlutusTx                   (BuiltinData, compile, liftCode, applyCode)
import PlutusTx.Prelude           (traceIfFalse, Bool, (&&), ($), (.))
import Plutus.V2.Ledger.Contexts  (txSignedBy)
import Plutus.V1.Ledger.Interval  (contains)
import Utils (wrapValidator)


{-# INLINABLE mkVestingValidator #-}

mkVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkVestingValidator beneficiary deadline () ctx =
  traceIfFalse "beneficiary's signature missing" signedByBeneficiary
  && traceIfFalse "deadline not reached" deadlineReached
    where
      txInfo :: TxInfo
      txInfo = scriptContextTxInfo ctx

      signedByBeneficiary :: Bool
      signedByBeneficiary = txSignedBy txInfo beneficiary

      deadlineReached :: Bool
      deadlineReached = contains (from deadline ) $ txInfoValidRange txInfo

wrappedMkVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedMkVestingValidator = wrapValidator . mkVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [||wrappedMkVestingValidator||]) `applyCode` liftCode beneficiary)
