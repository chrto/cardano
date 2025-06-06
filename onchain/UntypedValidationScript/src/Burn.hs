{-# LANGUAGE DataKinds #-}
-- Stops the automatic import of Prelude.
-- The standard Prelude isn't compatible with PlutusTx (Plutus Core can't understand it)
-- PlutusTx.Prelude has its own versions of things like Bool, Maybe, if, etc.
-- Because everything in validator has to be inlineable!!
{-# LANGUAGE NoImplicitPrelude #-}
-- Enables compile-time code generation.
-- Needed for compiling validator functions into on-chain Plutus Core.
{-# LANGUAGE TemplateHaskell #-}
-- In validator string is represented as BuiltinString (ByteString) not as list of Char.
{-# LANGUAGE OverloadedStrings #-}

module Burn  where

import qualified Plutus.V2.Ledger.Api as  PlutusV2
import           PlutusTx                 (BuiltinData, compile)
import           PlutusTx.Prelude         (traceError)
-- import           PlutusTx.Prelude         (error)
import           Utils                    (writeValidatorToFile)
import           Prelude                  (IO)

-- Untyped OnChain Burn validator
--                 DATUM          REDEEMER       SCRIPTCONTEXT
mkBurnValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkBurnValidator _ _ _ = traceError "it burns!!!"
-- mkBurnValidator _ _ _ = error ()
-- The mkBurnValidator function has to be INLINABLE!
-- To quote and compile it using compile.
{-# INLINABLE mkBurnValidator #-}

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(compile [|| mkBurnValidator ||])

-- Helper function.
-- Store validator into assets.
saveValidator :: IO()
saveValidator = writeValidatorToFile "./assets/burn.plutus" validator