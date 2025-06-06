import { CBORHex, LucidEvolution, PrivateKey, TxHash } from '@lucid-evolution/lucid';
import { AppError } from 'common/error';
import { CborHex as CborHexModel, PrivateKey as PrivateKeyModel } from 'model/cardano/cardano.types';
import { Either } from 'tsmonad';
import { toCborHex, toPrivateKey } from '../../modelFactory/typesFactory';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import { LucidError } from 'common/httpErrors';
import doer from 'utils/monad/either/do/doer';

const submitTransaction = (cbor: CBORHex) => (lucid: LucidEvolution): Promise<Either<AppError, TxHash>> =>
  lucid.wallet().submitTx(cbor)
    .then(Either.right<AppError, TxHash>)
    .catch(err => Either.left<AppError, TxHash>(new LucidError(err.message)));

const selectWalletFromPrivateKey = (privateKey: PrivateKey) => (lucid: LucidEvolution): void =>
  lucid.selectWallet.fromPrivateKey(privateKey);

export default (lucid: LucidEvolution) => (cbor: CborHexModel, privateKey: PrivateKeyModel): Promise<Either<AppError, TxHash>> =>
  Promise.resolve<Either<AppError, LucidEvolution>>(Either.right<AppError, LucidEvolution>(lucid))
    .then(doer({
      right: selectWalletFromPrivateKey(toPrivateKey(privateKey))
    }))
    .then(doer({
      right: (lucid) => {
        const tx = lucid.fromTx(cbor);
        console.log(tx.toTransaction().to_json());
      }
    }))
    .then(asyncBind(submitTransaction(toCborHex(cbor))));
