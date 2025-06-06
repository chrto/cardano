{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Utils.Conversions
    (validatorTestnetAddressBech32
    , validatorMainnetAddressBech32
    , posixTimeFromIso8601
    , posixTimeToIso8601
    , pubKeyHashFromVkeyFile
    , printPubKeyHashFromVkeyFile
    , pubKeyHashStringFromVkeyFile
    , posixTimeFromIso8601OrErr) where

import            Plutus.V2.Ledger.Api        (Validator, POSIXTime)
import            Cardano.Ledger.BaseTypes    (Network (Testnet, Mainnet))
import qualified  Cardano.Api.Shelley         as Api
import qualified  Data.Text                   as Text
import qualified  Cardano.Ledger.Credential   as Cred
import qualified  Utils.Serialise             as Utils
import qualified  Data.Time.Format.ISO8601    as TimeFormat
import qualified  Data.Time.Clock.POSIX       as TimePosix
import            Data.Functor                ((<&>))
import            Prelude                     (String, Maybe, fromRational
                                                , toRational, (>>=), ($), (<$>), (*), (/), (.), round, Num (fromInteger), FilePath, IO, Either (Left, Right), print, Show (show), otherwise)
import Data.Maybe (fromJust, isJust)
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
      <&> (*) 1000
      <&> round . (*) 1000
      <&> fromInteger


--- >>> posixTimeFromIso8601 "2025-02-16T12:03:17Z"
-- Just (POSIXTime {getPOSIXTime = 1739707397000000})

posixTimeFromIso8601OrErr :: String -> Either String POSIXTime
posixTimeFromIso8601OrErr iso8601Time =
  toEither $ posixTimeFromIso8601 iso8601Time
    where
      toEither :: Maybe POSIXTime -> Either String POSIXTime
      toEither mp
        | isJust mp = Right $ fromJust mp
        | otherwise = Left ""

--- >>> posixTimeFromIso8601OrErr "2025-02-16T12:03:17Z"
-- Right (POSIXTime {getPOSIXTime = 1739707397000000})

validatorHash :: Validator -> Api.ScriptHash
validatorHash = hashScript . Utils.validatorToScript

hashScript :: Api.PlutusScript Api.PlutusScriptV2 -> Api.ScriptHash
hashScript = Api.hashScript . Api.PlutusScript Api.PlutusScriptV2

pubKeyHashFromVkeyFile :: FilePath -> IO (Either (Api.FileError Api.TextEnvelopeError) (Api.Hash Api.PaymentKey))
pubKeyHashFromVkeyFile path =
   Api.readFileTextEnvelope (Api.AsVerificationKey Api.AsPaymentKey) path
    <&> (<$>) Api.verificationKeyHash
    -- <&> \paymentKeyOrError -> (<$>) Api.verificationKeyHash  paymentKeyOrError
    -- <&> \paymentKeyOrError -> paymentKeyOrError <&> Api.verificationKeyHash

-- pubKeyHashFromVkeyFile' :: FilePath -> IO (Either String PubKeyHash)
-- pubKeyHashFromVkeyFile' path =
--    Api.readFileTextEnvelope (Api.AsVerificationKey Api.AsPaymentKey) path
--     <&> (<$>) Api.verificationKeyHash
--     <&> \case
--       Left err -> Left $ Api.displayError err
--       Right x -> Right $ PubKeyHash $ BuiltinByteString $ Crypto.hashToBytes x

    -- <&> \paymentKeyOrError -> (<$>) Api.verificationKeyHash  paymentKeyOrError
    -- <&> \paymentKeyOrError -> paymentKeyOrError <&> Api.verificationKeyHash

pubKeyHashStringFromVkeyFile :: FilePath -> IO (Either String String)
pubKeyHashStringFromVkeyFile path =
  pubKeyHashFromVkeyFile path
    <&> \case
      Left err -> Left $ Api.displayError err
      Right pubKeyHash -> Right $ show pubKeyHash

printPubKeyHashFromVkeyFile :: FilePath -> IO ()
printPubKeyHashFromVkeyFile path =
  pubKeyHashFromVkeyFile path
    >>= \case
      Left err -> print $ Api.displayError err
      Right pubKeyHash -> print pubKeyHash

-- printPubKeyHashFromVkeyFile :: FilePath -> IO ()
-- printPubKeyHashFromVkeyFile path =
--   Api.readFileTextEnvelope (Api.AsVerificationKey Api.AsPaymentKey) path
--     >>= \case
--       Left err -> print $ Api.displayError err
--       Right vkey -> print $ Api.verificationKeyHash vkey
