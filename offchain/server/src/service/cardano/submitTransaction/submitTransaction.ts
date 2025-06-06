import { AppError } from 'common/error';
import { CardanoStorage } from 'storage/cardano/lucid/lucid.types';
import { Either } from 'tsmonad';
import { CborHex, PrivateKey } from 'model/cardano/cardano.types';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';

// TODO load from file of db
const loadPrivateKey = async (): Promise<Either<AppError, PrivateKey>> =>
  Promise.resolve(Either.left<AppError, PrivateKey>(new AppError('not.implemented', 'Missing private key!!')));

export default (cardanoStorage: CardanoStorage) => (transaction: CborHex): Promise<Either<AppError, CborHex>> =>
  loadPrivateKey()
    .then(asyncBind((privKey: PrivateKey) => cardanoStorage.submitTransaction(transaction, privKey)));
