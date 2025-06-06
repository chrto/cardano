import { AppError } from 'common/error';
import { CardanoStorage } from 'storage/cardano/lucid/lucid.types';
import { Either } from 'tsmonad';
import { AddressDetails } from 'model/cardano/addressDetails/addressDetails.types';
import { Address } from 'model/cardano/cardano.types';

export default (cardanoStorage: CardanoStorage) => (address: Address): Either<AppError, AddressDetails> =>
  cardanoStorage.getAddressDetail(address);
