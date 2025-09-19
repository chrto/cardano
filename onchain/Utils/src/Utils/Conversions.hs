{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Utils.Conversions
    (validatorTestnetAddressBech32
    , validatorMainnetAddressBech32
    , posixTimeFromIso8601
    , posixTimeToIso8601
    , pubKeyHashFromVkeyFile
    , pubKeyHashStringFromVkeyFile
    , posixTimeFromIso8601OrErr
    , tryReadAddress
    , bytesFromHex
    , stringFromByteString
    , stringToByteString) where

import            Plutus.V2.Ledger.Api        (Validator, POSIXTime, Credential (PubKeyCredential, ScriptCredential), StakingCredential (StakingPtr, StakingHash), Address (..), PubKeyHash (PubKeyHash), ValidatorHash (ValidatorHash))
import            Cardano.Ledger.BaseTypes    (Network (Testnet, Mainnet), TxIx (TxIx), CertIx (CertIx))
import qualified  Cardano.Api.Shelley         as Api
import qualified  Data.Text                   as Text
import qualified  Cardano.Ledger.Credential   as Cred
import qualified  Utils.Serialise             as Utils
import qualified  Data.Time.Format.ISO8601    as TimeFormat
import qualified  Data.Time.Clock.POSIX       as TimePosix
import qualified  Data.ByteString.Char8       as B8
import            Data.Functor                ((<&>))
import            Prelude                     (String, Maybe, fromRational
                                                , toRational, ($), (<$>), (*), (/), (.), round, Num (fromInteger), FilePath, IO, Either (Left, Right), Show (show), otherwise, fromIntegral)
import            Data.Maybe                  (fromJust, isJust, Maybe (..))
import            Cardano.Ledger.Crypto       (StandardCrypto)
import            Cardano.Ledger.Keys         (KeyHash(..))
import            Data.Text                   (pack)
import            Cardano.Crypto.Hash.Class   (hashToBytes)
import            PlutusTx.Builtins           (toBuiltin)
import            Cardano.Ledger.Hashes       (ScriptHash(..))
import            Plutus.V1.Ledger.Bytes      (bytes, fromHex)


validatorAddressBech32 :: Network -> Validator -> String
validatorAddressBech32 network validator =
  Text.unpack $
  Api.serialiseToBech32 $
  Api.ShelleyAddress
    network
    (Cred.ScriptHashObj $ Api.toShelleyScriptHash $ validatorHash validator)
    Cred.StakeRefNull

validatorTestnetAddressBech32 :: Validator -> String
validatorTestnetAddressBech32 = validatorAddressBech32 Testnet

validatorMainnetAddressBech32 :: Validator -> String
validatorMainnetAddressBech32 = validatorAddressBech32 Mainnet

posixTimeToIso8601 :: POSIXTime -> String
posixTimeToIso8601 t = TimeFormat.formatShow TimeFormat.iso8601Format
  $ TimePosix.posixSecondsToUTCTime
  $ fromRational
  $ toRational t / 1000

--- >>> posixTimeToIso8601 1739707397000
-- "2025-02-16T12:03:17Z"

posixTimeFromIso8601 :: String -> Maybe POSIXTime
posixTimeFromIso8601 iso8601Time =
    TimeFormat.formatParseM TimeFormat.iso8601Format iso8601Time
      <&> TimePosix.utcTimeToPOSIXSeconds
      <&> round . (*) 1000
      <&> fromInteger

--- >>> posixTimeFromIso8601 "2025-02-16T12:03:17Z"
-- Just (POSIXTime {getPOSIXTime = 1739707397000})

posixTimeFromIso8601OrErr :: String -> Either String POSIXTime
posixTimeFromIso8601OrErr iso8601Time =
  toEither $ posixTimeFromIso8601 iso8601Time
    where
      toEither :: Maybe POSIXTime -> Either String POSIXTime
      toEither mp
        | isJust mp = Right $ fromJust mp
        | otherwise = Left ""

--- >>> posixTimeFromIso8601OrErr "2025-02-16T12:03:17Z"
-- Right (POSIXTime {getPOSIXTime = 1739707397000})

validatorHash :: Validator -> Api.ScriptHash
validatorHash = hashScript . Utils.validatorToScript

hashScript :: Api.PlutusScript Api.PlutusScriptV2 -> Api.ScriptHash
hashScript = Api.hashScript . Api.PlutusScript Api.PlutusScriptV2

pubKeyHashFromVkeyFile :: FilePath -> IO (Either (Api.FileError Api.TextEnvelopeError) (Api.Hash Api.PaymentKey))
pubKeyHashFromVkeyFile path =
   Api.readFileTextEnvelope (Api.AsVerificationKey Api.AsPaymentKey) path
    <&> (<$>) Api.verificationKeyHash

--- >>> pubKeyHashFromVkeyFile "/home/../workspace/cardano/keys/alice.vkey"

-- pubKeyHashFromVkeyFile' :: FilePath -> IO (Either String PubKeyHash)
-- pubKeyHashFromVkeyFile' path =
--    Api.readFileTextEnvelope (Api.AsVerificationKey Api.AsPaymentKey) path
--     <&> (<$>) Api.verificationKeyHash
--     <&> \case
--       Left err -> Left $ Api.displayError err
--       Right x -> Right $ PubKeyHash $ toBuiltin $ Crypto.hashToBytes x

--- >>>pubKeyHashFromVkeyFile' "/home/../workspace/cardano/keys/alice.vkey"

pubKeyHashStringFromVkeyFile :: FilePath -> IO (Either String String)
pubKeyHashStringFromVkeyFile path =
  pubKeyHashFromVkeyFile path
    <&> \case
      Left err -> Left $ Api.displayError err
      Right pubKeyHash -> Right $ show pubKeyHash

--- >>> pubKeyHashStringFromVkeyFile "/home/../workspace/cardano/keys/alice.vkey"

credentialLedgerToPlutus :: Cred.Credential a StandardCrypto -> Credential
credentialLedgerToPlutus (Cred.ScriptHashObj (ScriptHash h)) = ScriptCredential $ ValidatorHash $ toBuiltin $ hashToBytes h
credentialLedgerToPlutus (Cred.KeyHashObj (KeyHash h))       = PubKeyCredential $ PubKeyHash $ toBuiltin $ hashToBytes h

stakeReferenceLedgerToPlutus :: Cred.StakeReference StandardCrypto -> Maybe StakingCredential
stakeReferenceLedgerToPlutus (Cred.StakeRefBase x)                                       =
    Just $ StakingHash $ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (Cred.StakeRefPtr (Cred.Ptr (Api.SlotNo x) (TxIx y) (CertIx z))) =
    Just $ StakingPtr (fromIntegral x) (fromIntegral y) (fromIntegral z)
stakeReferenceLedgerToPlutus Cred.StakeRefNull                                           =
    Nothing

tryReadAddress :: String -> Maybe Address
tryReadAddress x = case Api.deserialiseAddress Api.AsAddressAny $ pack x of
    Nothing                                          -> Nothing
    Just (Api.AddressByron _)                        -> Nothing
    Just (Api.AddressShelley (Api.ShelleyAddress _ p s)) -> Just Address
        { addressCredential        = credentialLedgerToPlutus p
        , addressStakingCredential = stakeReferenceLedgerToPlutus s
        }
--- >>> tryReadAddress "addr_test1wrdk050fs7r9..zmjsxnmud5v8le6vjh6k5jhvchuyj76"
-- Just (Address {addressCredential = ScriptCredential db67d1e987865a47..d3df1b461ff9d3257d5a92bb3, addressStakingCredential = Nothing})

bytesFromHex :: B8.ByteString -> Maybe B8.ByteString
bytesFromHex bs = case bytes <$> fromHex bs of
  Right pkh -> Just pkh
  Left _    -> Nothing

stringFromByteString :: B8.ByteString -> String
stringFromByteString = B8.unpack

stringToByteString :: String -> B8.ByteString
stringToByteString = B8.pack