{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}

module VestingUtils where
import Plutus.V2.Ledger.Api     (PubKeyHash (PubKeyHash, getPubKeyHash), Validator, POSIXTime)
import qualified Vesting
import qualified VestingParametrized
import           Utils (writeValidatorToFile, validatorTestnetAddressBech32
                      , validatorMainnetAddressBech32, printDataToJSON
                      , posixTimeFromIso8601, pubKeyHashStringFromVkeyFile
                      , posixTimeFromIso8601OrErr) --, pubKeyHashFromVkeyFile)
-- import Data.Functor             ((<&>))
--, pubKeyHashFromVkeyFile)
--, pubKeyHashFromVkeyFile)
-- import Data.Functor             ((<&>))
--, pubKeyHashFromVkeyFile)
--, pubKeyHashFromVkeyFile)
-- import Data.Functor             ((<&>))
--, pubKeyHashFromVkeyFile)
--, pubKeyHashFromVkeyFile)
-- import Data.Functor             ((<&>))
--, pubKeyHashFromVkeyFile)
import           Prelude (IO, String, (.), ($), (++), (.), (<*>), FilePath
                        , Either(Left, Right), print)
import Data.Maybe               (fromJust)
import PlutusTx.Builtins.Class  (stringToBuiltinByteString)
import Data.Functor

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

toPubKeyHash :: String -> PubKeyHash
toPubKeyHash s = PubKeyHash { getPubKeyHash = stringToBuiltinByteString s }

toPubKeyHash' :: String -> PubKeyHash
toPubKeyHash' = PubKeyHash . stringToBuiltinByteString

vestingDatumToJson :: PubKeyHash -> String -> IO ()
vestingDatumToJson pkh utcIso8601Time = printDataToJSON $ Vesting.VestingDatum
  {
      Vesting.beneficiary = pkh
    , Vesting.deadline = fromJust $ posixTimeFromIso8601 utcIso8601Time
  }

vestingDatumToJson' :: String -> String -> IO ()
vestingDatumToJson' pkh utcIso8601Time = printDataToJSON $ Vesting.VestingDatum
  {
      Vesting.beneficiary = toPubKeyHash pkh
    , Vesting.deadline = fromJust $ posixTimeFromIso8601 utcIso8601Time
  }

-- Parametrized Vesting
saveVestingParametrized :: FilePath -> VestingParametrized.VestingParams -> IO ()
saveVestingParametrized outFile = saveValidator outFile . VestingParametrized.validator

saveVestingParametrizedFor :: String -> String -> IO ()
saveVestingParametrizedFor beneficiary deadline = do
  pkhOrErr <- pubKeyHashStringFromVkeyFile vKeyPath
  let timeOrErr = posixTimeFromIso8601OrErr deadline
      paramsOrErr = buildVestingParams <$> pkhOrErr <*> timeOrErr
  saveParametrized paramsOrErr
    where
      vKeyPath :: FilePath
      vKeyPath = "./../../keys/" ++ beneficiary ++ ".vkey"

      outPath :: FilePath
      outPath = "./assets/" ++ beneficiary ++ "VestingParametrized.plutus"

      saveParametrized :: Either String VestingParametrized.VestingParams -> IO ()
      saveParametrized (Right params) = saveVestingParametrized outPath params
      saveParametrized (Left error)    = print error

      buildVestingParams :: String -> POSIXTime -> VestingParametrized.VestingParams
      buildVestingParams pkh time = VestingParametrized.VestingParams
        {
          VestingParametrized.beneficiary = toPubKeyHash pkh
          , VestingParametrized.deadline = time
        }

-- vestingDatumToJson'' :: FilePath -> String -> IO()
-- vestingDatumToJson'' vKeyFile utcIso8601Time =
--   pubKeyHashFromVkeyFile vKeyFile
--     <&> (<$>)
