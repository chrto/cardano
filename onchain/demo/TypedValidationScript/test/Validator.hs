{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Control.Monad          (replicateM, unless)
import qualified Plutus.Model           as Model
import qualified Plutus.V2.Ledger.Api   as LedgerApi
import           Test.Tasty             (TestTree, defaultMain, testGroup)
import           RedeemerBool           (validator')
import           RedeemerCustomBool     (MyRedeemer(MyRedeemer), validator)
import           Prelude

type RedeemerBoolType = Model.TypedValidator () (Bool, Bool)
type RedeemerCustomBoolType = Model.TypedValidator () MyRedeemer

script1 :: RedeemerBoolType
script1 = Model.TypedValidator $ Model.toV2 validator'


script2 :: RedeemerCustomBoolType
script2 = Model.TypedValidator $ Model.toV2 validator

setupUsers :: Model.Run [LedgerApi.PubKeyHash]
setupUsers = replicateM 3 $ Model.newUser $ Model.ada (Model.Lovelace 1000)

setupUsers' :: Model.Run [LedgerApi.PubKeyHash]
setupUsers' = do
  user1 <- Model.newUser $ Model.adaValue 1000
  user2 <- Model.newUser $ Model.adaValue 1000
  user3 <- Model.newUser $ Model.adaValue 1000
  pure [user1, user2, user3]

setupUsers'' :: Model.Run [LedgerApi.PubKeyHash]
setupUsers'' = replicateM 3 $ Model.newUser $ Model.adaValue 1000

--- >>> ada (Lovelace 1000)
-- Value (Map [(,Map [("",1000)])])
--- >>> adaValue 1000
-- Value (Map [(,Map [("",1000)])])


main :: IO ()
main = do
  defaultMain $ do
    testGroup "Typed Validator tests"
      [
        caseCustomBool Model.defaultBabbage
      , homework2 Model.defaultBabbage
      ]

caseCustomBool :: Model.MockConfig -> TestTree
caseCustomBool cfg = do
  testGroup
    "Testing Homework1"
    [ good "Case: (True, True)" $ giveGift (True, True)
    , bad "Case: (True, False)" $ giveGift (True, False)
    , bad "Case: (False, True)" $ giveGift (False, True)
    , bad "Case: (False, False)" $ giveGift (False, False)
    ]
  where
    bad msg = good msg . Model.mustFail
    good = Model.testNoErrors (Model.adaValue 10_000_000) cfg

giveGift :: (Bool, Bool) -> Model.Run ()
giveGift redeemer = do
  users <- setupUsers
  let [u1, u2, _u3] = users
      val = Model.adaValue 100
  Model.checkBalance (Model.gives u1 val script1) $ do
    sp <- Model.spend u1 val
    Model.submitTx u1 $ giveTx sp val

  utxos <- Model.utxoAt script1
  let [(giftRef, giftOut)] = utxos
  Model.checkBalance (Model.gives script1 (LedgerApi.txOutValue giftOut) u2) $ do
    Model.submitTx u2 $ takeTx u2 redeemer giftRef (LedgerApi.txOutValue giftOut)

  vals <- mapM Model.valueAt users
  let [v1, v2, _] = vals
  unless (v1 == Model.adaValue 900 && v2 == Model.adaValue 1100) $
    Model.logError "Final balances are incorrect"

giveTx :: Model.UserSpend -> LedgerApi.Value -> Model.Tx
giveTx usp val =
  Model.userSpend usp
  <> Model.payToScript script1 (Model.HashDatum ()) val

takeTx :: LedgerApi.PubKeyHash -> (Bool, Bool) -> LedgerApi.TxOutRef -> LedgerApi.Value -> Model.Tx
takeTx pkh redeemer giftRef giftVal =
   Model.spendScript script1 giftRef redeemer ()
   <> Model.payToKey pkh giftVal


homework2 :: Model.MockConfig -> TestTree
homework2 cfg = do
  testGroup
    "Testing Homework2"
    [ bad "Case: MyRedeemer True True" $ giveGift' (MyRedeemer True True)
    , good "Case: MyRedeemer True False" $ giveGift' (MyRedeemer True False)
    , good "Case: MyRedeemer False True" $ giveGift' (MyRedeemer False True)
    , bad "Case: MyRedeemer False False" $ giveGift' (MyRedeemer False False)
    ]
  where
    bad msg = good msg . Model.mustFail
    good = Model.testNoErrors (Model.adaValue 10_000_000) cfg

giveGift' :: MyRedeemer -> Model.Run ()
giveGift' redeemer = do
  users <- setupUsers
  let [u1, u2, _u3] = users
      val = Model.adaValue 100
  Model.checkBalance (Model.gives u1 val script2) $ do
    sp <- Model.spend u1 val
    Model.submitTx u1 $ giveTx' sp val

  utxos <- Model.utxoAt script2
  let [(giftRef, giftOut)] = utxos
  Model.checkBalance (Model.gives script2 (LedgerApi.txOutValue giftOut) u2) $ do
    Model.submitTx u2 $ takeTx' u2 redeemer giftRef (LedgerApi.txOutValue giftOut)

  vals <- mapM Model.valueAt users
  let [v1, v2, _] = vals
  unless (v1 == Model.adaValue 900 && v2 == Model.adaValue 1100) $
    Model.logError "Final balances are incorrect"

giveTx' :: Model.UserSpend -> LedgerApi.Value -> Model.Tx
giveTx' usp val =
  Model.userSpend usp
  <> Model.payToScript script2 (Model.HashDatum ()) val

takeTx' :: LedgerApi.PubKeyHash -> MyRedeemer -> LedgerApi.TxOutRef -> LedgerApi.Value -> Model.Tx
takeTx' pkh redeemer giftRef giftVal =
  Model.spendScript script2 giftRef redeemer ()
  <> Model.payToKey pkh giftVal
