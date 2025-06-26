import { AppError } from 'common/error';
import { CardanoStorage } from 'storage/cardano/lucid/lucid.types';
import { Either } from 'tsmonad';
import { Address, CborHex, Datum, Lovelace } from 'model/cardano/cardano.types';

export default (cardanoStorage: CardanoStorage) => (walletAddress: Address, contractAddress: Address, datum: Datum, amount: Lovelace): Promise<Either<AppError, CborHex>> =>
  cardanoStorage.buildTransactionFromAddress(walletAddress, contractAddress, datum, amount);
