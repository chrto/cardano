{-# LANGUAGE NoImplicitPrelude #-}

module Utils.PlutusTx (wrapValidator) where

import           Plutus.V2.Ledger.Api (ScriptContext)
import           PlutusTx (UnsafeFromData, BuiltinData, unsafeFromBuiltinData)
import           PlutusTx.Prelude (Bool, check, ($))

{-# INLINABLE wrapValidator #-}
wrapValidator :: (UnsafeFromData a, UnsafeFromData b)
              => (a -> b -> ScriptContext -> Bool)
              ->  (BuiltinData -> BuiltinData -> BuiltinData -> ())
wrapValidator f a b ctx = check
  $ f
    (unsafeFromBuiltinData a)
    (unsafeFromBuiltinData b)
    (unsafeFromBuiltinData ctx)
