import { CBORHex, LucidEvolution, PrivateKey, TxSignBuilder, TxSigned } from '@lucid-evolution/lucid';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { CborHex as CborHexModel, PrivateKey as PrivateKeyModel } from 'model/cardano/cardano.types';
import { fromCborHex, toCborHex, toPrivateKey } from '../../modelFactory/typesFactory';
import lift from 'utils/monad/either/lift/lift';
import bind from 'utils/monad/either/bind/bind';
import { LucidError } from 'common/httpErrors';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import eitherify from 'utils/monad/either/eitherify/eitherify';
import doer from 'utils/monad/either/do/doer';

const selectWalletFromPrivateKey = (privateKey: PrivateKey) => (lucid: LucidEvolution): void =>
  lucid.selectWallet.fromPrivateKey(privateKey);

const loadTransaction = (cbor: CBORHex) => (lucid: LucidEvolution): TxSignBuilder =>
  lucid.fromTx(cbor);

const signAndCompleteTransaction = (privateKeyte: PrivateKey) => (txSignBuilder: TxSignBuilder): Promise<Either<AppError, TxSigned>> =>
  txSignBuilder
    .sign.withPrivateKey(privateKeyte)
    .complete()
    .then(Either.right<AppError, TxSigned>)
    .catch(err => Either.left<AppError, TxSigned>(new LucidError(err.message)));

const toCBORHex = (txSignBuilder: TxSigned): CBORHex => txSignBuilder.toCBOR();

export default (lucid: LucidEvolution) => (cbor: CborHexModel, privateKey: PrivateKeyModel): Promise<Either<AppError, CborHexModel>> =>
  Promise.resolve<Either<AppError, LucidEvolution>>(Either.right<AppError, LucidEvolution>(lucid))
    .then(doer({
      right: selectWalletFromPrivateKey(toPrivateKey(privateKey))
    }))
    .then(lift(loadTransaction(toCborHex(cbor))))
    .then(asyncBind(signAndCompleteTransaction(toPrivateKey(privateKey))))
    .then(bind(eitherify(toCBORHex)))
    .then(lift(fromCborHex));