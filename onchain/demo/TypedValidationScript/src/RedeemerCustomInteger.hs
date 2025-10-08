{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module RedeemerCustomInteger where

import           Plutus.V2.Ledger.Api (Validator, mkValidatorScript
                                     , ScriptContext)
import           PlutusTx (compile, unstableMakeIsData, BuiltinData)
import           PlutusTx.Prelude (traceError, (==), ($), Integer, Bool(True)
                                 , otherwise, traceIfFalse)
import           Prelude (IO, Monad((>>)))
import           Utils (writeValidatorToFile, wrapValidator)

newtype MyRedeemer = MkMyRedeemer Integer

PlutusTx.unstableMakeIsData
  ''MyRedeemer -- Use TmplateHaskell to create an instance for IsData TypeClass

{-# INLINABLE mkGWRValidator #-}
mkGWRValidator :: () -> MyRedeemer -> ScriptContext -> Bool
mkGWRValidator _ (MkMyRedeemer redeemer) _ =
  if redeemer == 42
  then True
  else traceError "Wrong redeemer ;)"

wrappedMkGWRValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedMkGWRValidator = wrapValidator mkGWRValidator

validator :: Validator
validator = mkValidatorScript $$(compile [||wrappedMkGWRValidator||])

--

{-# INLINABLE mkGWRValidator' #-}
mkGWRValidator' :: () -> MyRedeemer -> ScriptContext -> Bool
mkGWRValidator' _ (MkMyRedeemer redeemer) _
  | redeemer == 42 = True
  | otherwise = traceError "Wrong redeemer ;)"

wrappedMkGWRValidator' :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedMkGWRValidator' = wrapValidator mkGWRValidator'

validator' :: Validator
validator' = mkValidatorScript $$(compile [||wrappedMkGWRValidator'||])

--

{-# INLINABLE mkGWRValidator'' #-}
mkGWRValidator'' :: () -> MyRedeemer -> ScriptContext -> Bool
mkGWRValidator'' _ (MkMyRedeemer 42) _ = True
mkGWRValidator'' _ _ _ = traceError "Wrong redeemer ;)"

wrappedMkGWRValidator'' :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedMkGWRValidator'' = wrapValidator mkGWRValidator''

validator'' :: Validator
validator'' = mkValidatorScript $$(compile [||wrappedMkGWRValidator''||])

--

{-# INLINABLE mkGWRValidator''' #-}
mkGWRValidator''' :: () -> MyRedeemer -> ScriptContext -> Bool
mkGWRValidator''' _ (MkMyRedeemer redeemer) _ =
  traceIfFalse "Wrong redeemer ;)" $ redeemer == 42

wrappedMkGWRValidator''' :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedMkGWRValidator''' = wrapValidator mkGWRValidator'''

validator''' :: Validator
validator''' = mkValidatorScript $$(compile [||wrappedMkGWRValidator'''||])

-- Serialize validator to file
saveValidator :: IO ()
saveValidator =
  writeValidatorToFile "./assets/redeemerCustomInteger.plutus" validator
  >> writeValidatorToFile "./assets/redeemerCustomIntegerP.plutus" validator'
  -- >> writeValidatorToFile "./assets/redeemerCustomIntegerPP.plutus" validator''
  >> writeValidatorToFile "./assets/redeemerCustomIntegerPPP.plutus" validator'''
