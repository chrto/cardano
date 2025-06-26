import { AppError } from 'common/error';
import { Address } from 'model/cardano/cardano.types';
import { Script } from 'model/cardano/script/script.types';
import { CardanoStorage } from 'storage/cardano/lucid/lucid.types';
import { Either } from 'tsmonad';

export default (cardanoStorage: CardanoStorage) => (script: Script): Either<AppError, Address> =>
  cardanoStorage.getValidatorAddress(script);
