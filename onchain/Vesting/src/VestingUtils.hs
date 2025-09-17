{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module VestingUtils where
import                Plutus.V2.Ledger.Api            (Validator, POSIXTime
                                                      , PubKeyHash (PubKeyHash), Address(addressCredential)
                                                      , Credential(PubKeyCredential, ScriptCredential)
                                                      , ValidatorHash, toBuiltin)
import qualified     Vesting
import              Plutus.V1.Ledger.Bytes           (bytes, fromHex)
import qualified     VestingParametrized
import qualified     VestingParametrizedTwo
import qualified     VestingParametrizedBeneficiary

import               Utils                            (writeValidatorToFile, validatorTestnetAddressBech32
                                                      , validatorMainnetAddressBech32, printDataToJSON
                                                      , posixTimeFromIso8601
                                                      , tryReadAddress, writeDataToFile, jsonToString, dataToJSON)

import              Prelude                           (IO, String, (.), ($), (++), (.), (<*>), FilePath
                                                      , Maybe, Either (Right, Left), (>>=), return, putStrLn)
import              Data.Maybe                        (Maybe (..))
import              Data.Functor                      ((<$>), (<&>))
import              Data.ByteString.Char8             (ByteString, pack, unpack)

-- Common
saveValidator :: FilePath -> Validator -> IO ()
saveValidator = writeValidatorToFile

-- Vesting
saveVesting :: IO ()
saveVesting = saveValidator "./assets/vesting.plutus" Vesting.validator

vestingTestnetAddressBech32 :: String
vestingTestnetAddressBech32 = validatorTestnetAddressBech32 Vesting.validator

vestingMainnetAddressBech32 :: String
vestingMainnetAddressBech32 = validatorMainnetAddressBech32 Vesting.validator

pubKeyHashFromAddress :: String -> Maybe PubKeyHash
pubKeyHashFromAddress address = tryReadAddress address
  >>= (\case
    PubKeyCredential pkh -> Just pkh
    ScriptCredential _ -> Nothing)
    . addressCredential

--- >>> pubKeyHashFromAddress "addr_test1vpy494af9z9th4anvcjnp8pxfsyfmkakqshaw6v784qph9qtutwc9"

pubKeyHashFromPkh :: String -> Maybe PubKeyHash
pubKeyHashFromPkh pkh = PubKeyHash . toBuiltin <$> hexOrErr pkh
  where hexOrErr = bytesFromHex . pack

pubKeyHashFromPkhBS :: ByteString -> Maybe PubKeyHash
pubKeyHashFromPkhBS pkh = PubKeyHash . toBuiltin <$> bytesFromHex pkh

bytesFromHex :: ByteString -> Maybe ByteString
bytesFromHex bs = case bytes <$> fromHex bs of
  Right pkh -> Just pkh
  Left _ -> Nothing

validatorHashFromAddress :: String -> Maybe ValidatorHash
validatorHashFromAddress address = tryReadAddress address
  >>= (\case
    ScriptCredential vh -> Just vh
    PubKeyCredential _ -> Nothing)
    . addressCredential
--- >>> validatorHashFromAddress "addr_test1wpmcnpr36xjk7exddlg3pcypc8u3hykvzjz0yjq2ldypjzs630u48"
-- Just 77898471d1a56f64cd6fd110e081c1f91b92cc1484f2480afb48190a

--- >>> validatorHashFromAddress "addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla"
-- Just 3a888d65f16790950a72daee1f63aa05add6d268434107cfa5b67712

getVestingDatumFromPkh :: String -> String -> Maybe Vesting.VestingDatum
getVestingDatumFromPkh pkh utcIso8601Time =
  Vesting.VestingDatum <$> pubKeyHashFromPkh pkh <*> posixTimeFromIso8601 utcIso8601Time

getVestingDatumFromPkhBS :: ByteString -> String -> Maybe Vesting.VestingDatum
getVestingDatumFromPkhBS pkh utcIso8601Time =
  Vesting.VestingDatum <$> pubKeyHashFromPkhBS pkh <*> posixTimeFromIso8601 utcIso8601Time

getVestingDatumFromAddr :: String -> String -> Maybe Vesting.VestingDatum
getVestingDatumFromAddr address utcIso8601Time =
  Vesting.VestingDatum <$> pubKeyHashFromAddress address <*> posixTimeFromIso8601 utcIso8601Time

--- >>> getVestingDatumFromAddr "addr_test1vpy494af9z9th4anvcjnp8pxfsyfmkakqshaw6v784qph9qtutwc9" "2025-02-16T12:03:17Z"
-- Just (VestingDatum {beneficiary = 4952d7a9288abbd7b36625309c264c089ddbb6042fd7699e3d401b94, deadline = POSIXTime {getPOSIXTime = 1739707397000}})

getVestingDatumFromAddr' :: String -> String -> Maybe Vesting.VestingDatum
getVestingDatumFromAddr' address utcIso8601Time =
  pubKeyHashFromAddress address
    >>= (\pkh -> posixTimeFromIso8601 utcIso8601Time
      <&> (\posixTime -> Vesting.VestingDatum {
            Vesting.beneficiary = pkh
            , Vesting.deadline = posixTime
      })
    )

--- >>> getVestingDatumFromAddr' "addr_test1vpy494af9z9th4anvcjnp8pxfsyfmkakqshaw6v784qph9qtutwc9" "2025-02-16T12:03:17Z"
-- No instance for (Show VestingDatum)
--   arising from a use of ‘evalPrint’

getVestingDatumFromAddr'' :: String -> String -> Maybe Vesting.VestingDatum
getVestingDatumFromAddr'' address utcIso8601Time = do
  pkh <- pubKeyHashFromAddress address
  posixTime <- posixTimeFromIso8601 utcIso8601Time

  return Vesting.VestingDatum {
    Vesting.beneficiary = pkh
    , Vesting.deadline = posixTime
  }

vestingDatumToJson :: Vesting.VestingDatum -> String
vestingDatumToJson  = jsonToString . dataToJSON

--- >>> vestingDatumToJson <$> getVestingDatumFromAddr "addr_test1vpy494af9z9th4anvcjnp8pxfsyfmkakqshaw6v784qph9qtutwc9" "2025-02-16T12:03:17Z"
-- Just "{\n    \"constructor\": 0,\n    \"fields\": [\n        {\n            \"bytes\": \"4952d7a9288abbd7b36625309c264c089ddbb6042fd7699e3d401b94\"\n        },\n        {\n            \"int\": 1739707397000\n        }\n    ]\n}"

vestingRedeemerToJson :: String
vestingRedeemerToJson = jsonToString $ dataToJSON ()

--- >>> vestingRedeemerToJson
-- "{\n    \"constructor\": 0,\n    \"fields\": []\n}"

printVestingDatumFromAddr :: String -> String -> IO()
printVestingDatumFromAddr address utcIso8601Time =
  case getVestingDatumFromAddr address utcIso8601Time of
    Just datum -> printVestingDatum datum
    Nothing -> putStrLn $ "Can not create datum from '" ++ address ++ "' and '" ++ utcIso8601Time ++ "'!"

printVestingDatumFromPkh :: String -> String -> IO()
printVestingDatumFromPkh pkh utcIso8601Time =
  case getVestingDatumFromPkh pkh utcIso8601Time of
    Just datum -> printVestingDatum datum
    Nothing -> putStrLn $ "Can not create datum from '" ++ pkh ++ "' and '" ++ utcIso8601Time ++ "'!"

printVestingDatumFromPkhBS :: ByteString -> String -> IO()
printVestingDatumFromPkhBS pkh utcIso8601Time =
  case getVestingDatumFromPkhBS pkh utcIso8601Time of
    Just datum -> printVestingDatum datum
    Nothing -> putStrLn $ "Can not create datum from '" ++ unpack pkh ++ "' and '" ++ utcIso8601Time ++ "'!"

printVestingDatum :: Vesting.VestingDatum -> IO ()
printVestingDatum = printDataToJSON

printVestingRedeemer :: IO()
printVestingRedeemer = printDataToJSON ()

saveVestingDatumFromAddr :: String -> String -> IO ()
saveVestingDatumFromAddr address utcIso8601Time =
  case getVestingDatumFromAddr address utcIso8601Time of
    Just datum -> saveVestingDatum datum
    Nothing -> putStrLn $ "Can not create datum from '" ++ address ++ "' and '" ++ utcIso8601Time ++ "'!"

saveVestingDatumFromPkh :: String -> String -> IO ()
saveVestingDatumFromPkh pkh utcIso8601Time =
  case getVestingDatumFromPkh pkh utcIso8601Time of
    Just datum -> saveVestingDatum datum
    Nothing -> putStrLn $ "Can not create datum from '" ++ pkh ++ "' and '" ++ utcIso8601Time ++ "'!"

saveVestingDatumFromPkhBS :: ByteString -> String -> IO ()
saveVestingDatumFromPkhBS pkh utcIso8601Time =
  case getVestingDatumFromPkhBS pkh utcIso8601Time of
    Just datum -> saveVestingDatum datum
    Nothing -> putStrLn $ "Can not create datum from '" ++ unpack pkh ++ "' and '" ++ utcIso8601Time ++ "'!"

saveVestingDatum :: Vesting.VestingDatum -> IO ()
saveVestingDatum = writeDataToFile "./assets/vesting-datum.json"

saveVestingRedeemer :: IO()
saveVestingRedeemer = writeDataToFile "./assets/unit.json" ()

------------------------------------
-- save validators
-- Parametrized Vesting
saveVestingParametrized :: FilePath -> VestingParametrized.VestingParams -> IO ()
saveVestingParametrized outFile = saveValidator outFile . VestingParametrized.validator

-- Parametrized Vesting
saveVestingParametrizedTwo :: FilePath -> PubKeyHash -> POSIXTime -> IO ()
saveVestingParametrizedTwo outFile pkh deadline = saveValidator outFile $ VestingParametrizedTwo.validator pkh deadline

------------------------------------
-- Parametrized Vesting Beneficiary
saveVestingParametrizedBeneficiary :: FilePath -> PubKeyHash -> IO ()
saveVestingParametrizedBeneficiary outFile = saveValidator outFile . VestingParametrizedBeneficiary.validator

--- >>> saveVestingParametrizedBeneficiary "./assets/vestingParametrizedAliceBeneficiary.plutus" "..."

vestingBeneficiaryTestnetAddressBech32 :: PubKeyHash -> String
vestingBeneficiaryTestnetAddressBech32 = validatorTestnetAddressBech32 . VestingParametrizedBeneficiary.validator
--- >>> vestingBeneficiaryTestnetAddressBech32 "..."

vestingBeneficiaryDatumToJson :: POSIXTime -> String
vestingBeneficiaryDatumToJson  = jsonToString . dataToJSON

printVestingBeneficiaryDatum :: POSIXTime -> IO ()
printVestingBeneficiaryDatum = printDataToJSON

saveVestingBeneficiaryDatum :: POSIXTime -> IO ()
saveVestingBeneficiaryDatum = writeDataToFile "./assets/vesting-beneficiary-datum.json"
