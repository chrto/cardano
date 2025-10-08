{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module RedeemerUntyped where

import           Plutus.V2.Ledger.Api (Validator, mkValidatorScript)
import           PlutusTx (BuiltinData, compile)
import           PlutusTx.Prelude (traceError, (==), otherwise)
import           PlutusTx.Builtins (mkI)
import           Prelude (IO)
import           Utils (writeValidatorToFile)

{-# INLINABLE mkGWRValidator #-}
mkGWRValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkGWRValidator _ redeemer _ = if redeemer == mkI 42
                              then ()
                              else traceError "expected 42"

{-# INLINABLE mkGWRValidator' #-}
mkGWRValidator' :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkGWRValidator' _ redeemer _
  | redeemer == mkI 42 = ()
  | otherwise = traceError "expected 42"

validator :: Validator
validator = mkValidatorScript $$(compile [||mkGWRValidator||])

validator' :: Validator
validator' = mkValidatorScript $$(compile [||mkGWRValidator'||])

-- Serialize validator to file
saveValidator :: IO ()
saveValidator =
  writeValidatorToFile "./assets/redeemerUntyped.plutus" validator

saveValidator' :: IO ()
saveValidator' =
  writeValidatorToFile "./assets/redeemerUntypedP.plutus" validator'


