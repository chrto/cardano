{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Vesting where
import Plutus.V2.Ledger.Api       (Validator, PubKeyHash, POSIXTime, ScriptContext (scriptContextTxInfo), TxInfo (txInfoValidRange), from, mkValidatorScript)
import PlutusTx                   (unstableMakeIsData, BuiltinData, compile)
import PlutusTx.Prelude           (traceIfFalse, Bool, (&&), ($))
import Plutus.V2.Ledger.Contexts  (txSignedBy)
import Plutus.V1.Ledger.Interval  (contains)
import Utils (wrapValidator)


data VestingDatum = VestingDatum
  {
    beneficiary :: PubKeyHash
    , deadline  :: POSIXTime
  }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}

mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator datum () ctx =
  traceIfFalse "beneficiary's signature missing" signedByBeneficiary
  && traceIfFalse "deadline not reached" deadlineReached
    where
      txInfo :: TxInfo
      txInfo = scriptContextTxInfo ctx

      signedByBeneficiary :: Bool
      signedByBeneficiary = txSignedBy txInfo $ beneficiary datum

      deadlineReached :: Bool
      deadlineReached = contains (from $ deadline datum) $ txInfoValidRange txInfo

wrappedMkVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedMkVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [||wrappedMkVestingValidator||])
