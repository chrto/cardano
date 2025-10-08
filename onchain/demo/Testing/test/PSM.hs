module Main (main) where

import           Test.Tasty (defaultMain, testGroup, TestTree)
import           Plutus.Model (adaValue, defaultBabbage, testNoErrors, Run
                             , mustFail, newUser, ada, Ada(Lovelace), sendValue
                             , noErrors, valueAt)
import           Plutus.V2.Ledger.Api (PubKeyHash)
import           Control.Monad (replicateM)

main :: IO ()
main = defaultMain
  $ do
    testGroup
      "Test demo"
      [ happyPath "Should send some founds between users" sendSomeFounds
      , errorPath "Should fail, if not enough founds" notEnoughFunds]
  where
    errorPath :: String -> Run a -> TestTree
    errorPath msg = happyPath msg . mustFail

    happyPath :: String -> Run a -> TestTree
    happyPath = testNoErrors (adaValue 10000000) defaultBabbage

setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ ada $ Lovelace 1000

-- setupUsers = replicateM 3 $ newUser $ adaValue 1000
sendSomeFounds :: Run Bool
sendSomeFounds = do
  users <- setupUsers
  let [u1, u2, u3] = users
  sendValue u1 (adaValue 100) u2
  sendValue u2 (adaValue 100) u3
  isOk <- noErrors
  balance <- mapM valueAt users
  let expected = adaValue <$> [900, 1000, 1100]
  return (isOk && balance == expected)

notEnoughFunds :: Run Bool
notEnoughFunds = do
  users <- setupUsers
  let [u1, u2, _] = users
  sendValue u1 (adaValue 2000) u2
  noErrors
