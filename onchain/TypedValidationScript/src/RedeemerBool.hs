{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module RedeemerBool where

import           Plutus.V2.Ledger.Api (Validator, mkValidatorScript
                                     , ScriptContext)
import           PlutusTx (compile)
import           PlutusTx.Prelude (traceError, ($), (&&), Bool(True)
                                 , otherwise, traceIfFalse)
import           Prelude (IO, Monad((>>)))
import           Utils (writeValidatorToFile, wrapValidator)

-- This should validate if and only if the two Booleans in the redeemer are True!
{-# INLINABLE mkGWRValidator #-}
mkGWRValidator :: () -> (Bool, Bool) -> ScriptContext -> Bool
mkGWRValidator _ (x, y) _ =
  traceIfFalse "Wrong redeemer ;)" $ x && y

validator :: Validator
validator =
  mkValidatorScript $$(compile [||wrapValidator mkGWRValidator||])

--

{-# INLINABLE mkGWRValidator' #-}
mkGWRValidator' :: () -> (Bool, Bool) -> ScriptContext -> Bool
mkGWRValidator' _ (x, y) _
  | x && y = True
  | otherwise = traceError "Wrong redeemer ;)"

validator' :: Validator
validator' = mkValidatorScript $$(compile [||wrapValidator mkGWRValidator'||])

--

{-# INLINABLE mkGWRValidator'' #-}
mkGWRValidator'' :: () -> (Bool, Bool) -> ScriptContext -> Bool
mkGWRValidator'' _ (True, True) _ = True
mkGWRValidator'' _ _ _ = traceError "Wrong redeemer ;)"

validator'' :: Validator
validator'' =
  mkValidatorScript $$(compile [||wrapValidator mkGWRValidator''||])

--

{-# INLINABLE mkGWRValidator''' #-}
mkGWRValidator''' :: () -> (Bool, Bool) -> ScriptContext -> Bool
mkGWRValidator''' _ (x, y) _ = if x && y
                              then True
                              else traceError "Wrong redeemer ;)"

validator''' :: Validator
validator''' = mkValidatorScript $$(compile [||wrapValidator mkGWRValidator'''||])


-- Serialize validator to file
saveValidator :: IO ()
saveValidator =
  writeValidatorToFile "./assets/redeemerBool.plutus" validator
  >> writeValidatorToFile "./assets/redeemerBoolP.plutus" validator'
  >> writeValidatorToFile "./assets/redeemerBoolPPP.plutus" validator'''
  >> writeValidatorToFile "./assets/redeemerBoolPP.plutus" validator''
