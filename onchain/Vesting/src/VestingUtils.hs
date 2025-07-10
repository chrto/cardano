{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module VestingUtils where
import                           Plutus.V2.Ledger.Api     (PubKeyHash (PubKeyHash, getPubKeyHash), Validator, POSIXTime (POSIXTime), Address (addressCredential), Credential (PubKeyCredential, ScriptCredential), ValidatorHash)
import qualified                 Vesting
import qualified                 VestingParametrized

import                           Utils                    (writeValidatorToFile, validatorTestnetAddressBech32
                                                            , validatorMainnetAddressBech32, printDataToJSON
                                                            , posixTimeFromIso8601, pubKeyHashStringFromVkeyFile
                                                            , posixTimeFromIso8601OrErr, tryReadAddress, writeDataToFile, jsonToString, dataToJSON)

import                           Prelude                  (IO, String, (.), ($), (++), (.), (<*>), FilePath
                                                            , Either(Left, Right), print, Maybe, (>>=), Integer, return, putStrLn)
import Data.Maybe                                         (fromJust, Maybe (..))
import PlutusTx.Builtins.Class                            (stringToBuiltinByteString)
import           Data.Functor                             ((<$>), (<&>))

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
-- Just 4952d7a9288abbd7b36625309c264c089ddbb6042fd7699e3d401b94

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

getVestingDatum :: String -> String -> Maybe Vesting.VestingDatum
getVestingDatum address utcIso8601Time =
  Vesting.VestingDatum <$> pubKeyHashFromAddress address <*> posixTimeFromIso8601 utcIso8601Time

--- >>> getVestingDatum "addr_test1vpy494af9z9th4anvcjnp8pxfsyfmkakqshaw6v784qph9qtutwc9" "2025-02-16T12:03:17Z"
-- Just (VestingDatum {beneficiary = 4952d7a9288abbd7b36625309c264c089ddbb6042fd7699e3d401b94, deadline = POSIXTime {getPOSIXTime = 1739707397000}})

getVestingDatum' :: String -> String -> Maybe Vesting.VestingDatum
getVestingDatum' address utcIso8601Time =
  pubKeyHashFromAddress address
    >>= (\pkh -> posixTimeFromIso8601 utcIso8601Time
      <&> (\posixTime -> Vesting.VestingDatum {
            Vesting.beneficiary = pkh
            , Vesting.deadline = posixTime
      })
    )

--- >>> getVestingDatum' "addr_test1vpy494af9z9th4anvcjnp8pxfsyfmkakqshaw6v784qph9qtutwc9" "2025-02-16T12:03:17Z"
-- Just (VestingDatum {beneficiary = 4952d7a9288abbd7b36625309c264c089ddbb6042fd7699e3d401b94, deadline = POSIXTime {getPOSIXTime = 1739707397000}})

getVestingDatum'' :: String -> String -> Maybe Vesting.VestingDatum
getVestingDatum'' address utcIso8601Time = do
  pkh <- pubKeyHashFromAddress address
  posixTime <- posixTimeFromIso8601 utcIso8601Time

  return Vesting.VestingDatum {
    Vesting.beneficiary = pkh
    , Vesting.deadline = posixTime
  }

vestingDatumToJson :: Vesting.VestingDatum -> String
vestingDatumToJson  = jsonToString . dataToJSON

--- >>> vestingDatumToJson <$> getVestingDatum "addr_test1vpy494af9z9th4anvcjnp8pxfsyfmkakqshaw6v784qph9qtutwc9" "2025-02-16T12:03:17Z"
-- Just "{\n    \"constructor\": 0,\n    \"fields\": [\n        {\n            \"bytes\": \"4952d7a9288abbd7b36625309c264c089ddbb6042fd7699e3d401b94\"\n        },\n        {\n            \"int\": 1739707397000\n        }\n    ]\n}"

vestingRedeemerToJson :: String
vestingRedeemerToJson = jsonToString $ dataToJSON ()

--- >>> vestingRedeemerToJson
-- "{\n    \"constructor\": 0,\n    \"fields\": []\n}"

printVestingDatum :: String -> String -> IO()
printVestingDatum address utcIso8601Time =
  case getVestingDatum address utcIso8601Time of
    Just datum -> printDataToJSON datum
    Nothing -> putStrLn $ "Can not create datum from '" ++ address ++ "' and '" ++ utcIso8601Time ++ "'!"

printVestingDatum' :: Vesting.VestingDatum -> IO ()
printVestingDatum' = printDataToJSON

printVestingRedeemer :: IO()
printVestingRedeemer = printDataToJSON ()

saveVestingDatum :: String -> String -> IO ()
saveVestingDatum address utcIso8601Time =
  case getVestingDatum address utcIso8601Time of
    Just datum -> writeDataToFile "./assets/vesting-datum.json" datum
    Nothing -> putStrLn $ "Can not create datum from '" ++ address ++ "' and '" ++ utcIso8601Time ++ "'!"

saveVestingDatum'' :: Vesting.VestingDatum -> IO ()
saveVestingDatum'' = writeDataToFile "./assets/vesting-datum.json"

saveVestingRedeemer :: IO()
saveVestingRedeemer = writeDataToFile "./assets/unit.json" ()

-- Parametrized Vesting
saveVestingParametrized :: FilePath -> VestingParametrized.VestingParams -> IO ()
saveVestingParametrized outFile = saveValidator outFile . VestingParametrized.validator

-- saveVestingParametrizedFor :: String -> String -> IO ()
-- saveVestingParametrizedFor beneficiary deadline = do
--   pkhOrErr <- pubKeyHashStringFromVkeyFile vKeyPath
--   let timeOrErr = posixTimeFromIso8601OrErr deadline
--       paramsOrErr = buildVestingParams <$> pkhOrErr <*> timeOrErr
--   saveParametrized paramsOrErr
--     where
--       vKeyPath :: FilePath
--       vKeyPath = "./../../keys/" ++ beneficiary ++ ".vkey"

--       outPath :: FilePath
--       outPath = "./assets/" ++ beneficiary ++ "VestingParametrized.plutus"

--       saveParametrized :: Either String VestingParametrized.VestingParams -> IO ()
--       saveParametrized (Right params) = saveVestingParametrized outPath params
--       saveParametrized (Left error)    = print error

--       buildVestingParams :: String -> POSIXTime -> VestingParametrized.VestingParams
--       buildVestingParams pkh time = VestingParametrized.VestingParams
--         {
--           VestingParametrized.beneficiary = toPubKeyHash pkh
--           , VestingParametrized.deadline = time
--         }

--- >>> getVestingDatum' "addr_test1wpmcnpr36xj..yjq2ldypjzs630u48" "2025-02-16T12:03:17Z"
-- Nothing


