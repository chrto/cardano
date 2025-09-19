{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Utils.Helpers
  (
    pubKeyHashFromAddress
    , pubKeyHashFromPkh
    , pubKeyHashFromPkhBS
  ) where

import            Utils.Conversions (tryReadAddress, bytesFromHex, stringToByteString)
import            Plutus.V2.Ledger.Api (PubKeyHash (PubKeyHash), Credential (PubKeyCredential, ScriptCredential), Address (addressCredential), toBuiltin)
import            Prelude (String, Maybe(..), (>>=), (<$>), (.))
import qualified  Data.ByteString.Char8  as B8

pubKeyHashFromAddress :: String -> Maybe PubKeyHash
pubKeyHashFromAddress address = tryReadAddress address
  >>= (\case
    PubKeyCredential pkh -> Just pkh
    ScriptCredential _ -> Nothing)
    . addressCredential

--- >>> pubKeyHashFromAddress "addr_test1vpy494af9z9th4anvcjnp8pxfsyfmkakqshaw6v784qph9qtutwc9"

pubKeyHashFromPkh :: String -> Maybe PubKeyHash
pubKeyHashFromPkh pkh = PubKeyHash . toBuiltin <$> hexOrErr pkh
  where hexOrErr = bytesFromHex . stringToByteString

pubKeyHashFromPkhBS :: B8.ByteString -> Maybe PubKeyHash
pubKeyHashFromPkhBS pkh = PubKeyHash . toBuiltin <$> bytesFromHex pkh