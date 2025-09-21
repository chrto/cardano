{-# LANGUAGE LambdaCase #-}

module Utils.Serialise
    ( writeValidatorToFile
    , writeDataToFile
    , validatorToScript
    , printDataToJSON
    , jsonToString
    , dataToJSON
    , codeToScript
    , writeCodeToFile) where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           Cardano.Api.Shelley (PlutusScript(PlutusScriptSerialised)
                                    , PlutusScriptV2
                                    ,Error(displayError)
                                    ,writeFileTextEnvelope
                                    , fromPlutusData, writeFileJSON
                                    , prettyPrintJSON
                                    , scriptDataToJsonDetailedSchema)
import           PlutusTx           (CompiledCode)

import           Data.Aeson (Value)
import qualified Codec.Serialise as CodecSerialize
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Char8 as BS8
import           Text.Printf (printf)

-- Create file with compiled Plutus validator
writeValidatorToFile :: FilePath -> PlutusV2.Validator -> IO ()
writeValidatorToFile filePath = writeScriptToFile filePath . validatorToScript

-- Create file with Plutus script
writeScriptToFile :: FilePath -> PlutusScript PlutusScriptV2 -> IO ()
writeScriptToFile filePath script =
  writeFileTextEnvelope filePath Nothing script
  >>= \case
    Left err -> print $ "Script has not been serialised into file" ++ displayError err
    Right () -> putStrLn $ "Serialized script to: " ++ filePath

-- Create file with compiled code
writeCodeToFile :: FilePath -> CompiledCode a -> IO ()
writeCodeToFile filePath = writeScriptToFile filePath . codeToScript

-- Serialize validator
validatorToScript :: PlutusV2.Validator -> PlutusScript PlutusScriptV2
validatorToScript = serializableToScript

-- Serialize compiled code
codeToScript :: CompiledCode a -> PlutusScript PlutusScriptV2
codeToScript = serializableToScript . PlutusV2.fromCompiledCode

serializableToScript :: CodecSerialize.Serialise a => a -> PlutusScript PlutusScriptV2
serializableToScript = PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . CodecSerialize.serialise

writeDataToFile :: PlutusV2.ToData a => FilePath -> a -> IO ()
writeDataToFile filePath myData = do
  let json = dataToJSON myData
  writeFileJSON filePath json
    >>= \case
      Left err -> print $ displayError err
      Right () -> printf "Wrote data to: %s\n%s\n" filePath $ jsonToString json

printDataToJSON :: PlutusV2.ToData a => a -> IO ()
printDataToJSON = putStrLn . jsonToString . dataToJSON

jsonToString :: Value -> String
jsonToString = BS8.unpack . prettyPrintJSON

dataToJSON :: PlutusV2.ToData a => a -> Value
dataToJSON = scriptDataToJsonDetailedSchema . fromPlutusData . PlutusV2.toData
