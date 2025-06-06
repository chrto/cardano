{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module RedeemerInteger where

import           Plutus.V2.Ledger.Api (Validator, mkValidatorScript
                                     , ScriptContext)
import           PlutusTx (compile)
import           PlutusTx.Prelude (traceError, (==), ($), Integer, Bool(True)
                                 , otherwise, traceIfFalse)
import           Prelude (IO, Monad((>>)))
import           Utils (writeValidatorToFile, wrapValidator)

{-# INLINABLE mkGWRValidator #-}
mkGWRValidator :: () -> Integer -> ScriptContext -> Bool
mkGWRValidator _ redeemer _ = traceIfFalse "expected 42" $ redeemer == 42

validator :: Validator
validator =
  mkValidatorScript $$(compile [||wrapValidator mkGWRValidator||])

--

{-# INLINABLE mkGWRValidator' #-}
mkGWRValidator' :: () -> Integer -> ScriptContext -> Bool
mkGWRValidator' _ redeemer _ = if redeemer == 42
                              then True
                              else traceError "Wrong redeemer ;)"

validator' :: Validator
validator' = mkValidatorScript $$(compile [||wrapValidator mkGWRValidator'||])

--

{-# INLINABLE mkGWRValidator'' #-}
mkGWRValidator'' :: () -> Integer -> ScriptContext -> Bool
mkGWRValidator'' _ redeemer _
  | redeemer == 42 = True
  | otherwise = traceError "Wrong redeemer ;)"

validator'' :: Validator
validator'' = mkValidatorScript $$(compile [||wrapValidator mkGWRValidator''||])

-- -- Serialize validator to file
saveValidator :: IO ()
saveValidator =
  writeValidatorToFile "./assets/redeemerInteger.plutus" validator
  >> writeValidatorToFile "./assets/redeemerIntegerP.plutus" validator'
  >> writeValidatorToFile "./assets/redeemerIntegerPP.plutus" validator''
