{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module VestingParametrized where
import Plutus.V2.Ledger.Api       (Validator, PubKeyHash, POSIXTime, ScriptContext (scriptContextTxInfo), TxInfo (txInfoValidRange), from, mkValidatorScript)
import PlutusTx                   (BuiltinData, compile, liftCode, applyCode, makeLift) --, applyCode, Lift(liftCode))
import PlutusTx.Prelude           (traceIfFalse, Bool, (&&), ($), (.))
import Plutus.V2.Ledger.Contexts  (txSignedBy)
import Plutus.V1.Ledger.Interval  (contains)
import Utils (wrapValidator)


data VestingParams = VestingParams
  {
    beneficiary :: PubKeyHash
    , deadline  :: POSIXTime
  }

makeLift ''VestingParams


{-# INLINABLE mkVestingValidator #-}

mkVestingValidator :: VestingParams -> () -> () -> ScriptContext -> Bool
mkVestingValidator params () () ctx =
  traceIfFalse "beneficiary's signature missing" signedByBeneficiary
  && traceIfFalse "deadline not reached" deadlineReached
    where
      txInfo :: TxInfo
      txInfo = scriptContextTxInfo ctx

      signedByBeneficiary :: Bool
      signedByBeneficiary = txSignedBy txInfo $ beneficiary params

      deadlineReached :: Bool
      deadlineReached = contains (from $ deadline params ) $ txInfoValidRange txInfo

wrappedMkVestingValidator :: VestingParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedMkVestingValidator = wrapValidator . mkVestingValidator

validator :: VestingParams -> Validator
validator params = mkValidatorScript ($$(compile [||wrappedMkVestingValidator||]) `applyCode` liftCode params)
-- this will be compiled but not serialized. Error is "*** Exception: Error: Reference to a name which is not a local, a builtin, or an external INLINABLE function: Variable params
--  No unfolding
-- Because of VestingParams in known only on runtime, not in compile time!!!
-- Context: Compiling expr at "Vesting-0.1.0.0-inplace:VestingParametrized:(43,41)-(43,87)""
-- validator params = mkValidatorScript $$(compile [||wrappedMkVestingValidator params||])
