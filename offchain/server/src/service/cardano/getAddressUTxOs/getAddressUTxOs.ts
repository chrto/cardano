import { AppError } from 'common/error';
import { Address } from 'model/cardano/cardano.types';
import { UTxO } from 'model/cardano/utxo/utxo.types';
import { CardanoStorage } from 'storage/cardano/lucid/lucid.types';
import { Either } from 'tsmonad';

export default (cardanoStorage: CardanoStorage) => async (address: Address): Promise<Either<AppError, UTxO[]>> =>
  cardanoStorage.getAddressUTxOs(address);
