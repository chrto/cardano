{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module RedeemerCustomBool where

import           Plutus.V2.Ledger.Api (Validator, mkValidatorScript
                                     , ScriptContext)
import           PlutusTx (BuiltinData, compile, unstableMakeIsData)
import           PlutusTx.Prelude (traceError, (/=), Bool(True)
                                 , otherwise, traceIfFalse)
import           Prelude (IO, Monad((>>)))
import           Utils (writeValidatorToFile, wrapValidator)

data MyRedeemer = MyRedeemer
    { flag1 :: Bool
    , flag2 :: Bool
    }

PlutusTx.unstableMakeIsData ''MyRedeemer

-- Create a validator that unlocks the funds if MyRedemeer's flags are different
{-# INLINABLE mkValidator #-}
mkValidator :: () -> MyRedeemer -> ScriptContext -> Bool
mkValidator () r _ = if flag1 r /= flag2 r
                              then True
                              else traceError "Wrong redeemer ;)"

{-# INLINABLE wrappedValidator #-}
wrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator = wrapValidator mkValidator

validator :: Validator
validator = mkValidatorScript $$(compile [||wrappedValidator||])

--

{-# INLINABLE mkValidator' #-}
mkValidator' :: () -> MyRedeemer -> ScriptContext -> Bool
mkValidator' () MyRedeemer {flag1, flag2} _ = if flag1 /= flag2
                              then True
                              else traceError "Wrong redeemer ;)"

{-# INLINABLE wrappedValidator' #-}
wrappedValidator' :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator' = wrapValidator mkValidator'

validator' :: Validator
validator' = mkValidatorScript $$(compile [||wrappedValidator'||])

--

{-# INLINABLE mkValidator'' #-}
mkValidator'' :: () -> MyRedeemer -> ScriptContext -> Bool
mkValidator'' () MyRedeemer {flag1, flag2} _
  | flag1 /= flag2  = True
  | otherwise       = traceError "Wrong redeemer ;)"

{-# INLINABLE wrappedValidator'' #-}
wrappedValidator'' :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator'' = wrapValidator mkValidator''

validator'' :: Validator
validator'' = mkValidatorScript $$(compile [||wrappedValidator''||])

--

{-# INLINABLE mkValidator''' #-}
mkValidator''' :: () -> MyRedeemer -> ScriptContext -> Bool
mkValidator''' () MyRedeemer {flag1, flag2} _ = traceIfFalse "Wrong redeemer ;)" flag1 /= flag2

{-# INLINABLE wrappedValidator''' #-}
wrappedValidator''' :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator''' = wrapValidator mkValidator'''

validator''' :: Validator
validator''' = mkValidatorScript $$(compile [||wrappedValidator'''||])

-- -- -- Serialize validator to file
saveValidator :: IO ()
saveValidator =
  writeValidatorToFile "./assets/redeemerCustomBool.plutus" validator
  >> writeValidatorToFile "./assets/redeemerCustomBoolP.plutus" validator'
  >> writeValidatorToFile "./assets/redeemerCustomBoolPP.plutus" validator''
  >> writeValidatorToFile "./assets/redeemerCustomBoolPPP.plutus" validator'''
