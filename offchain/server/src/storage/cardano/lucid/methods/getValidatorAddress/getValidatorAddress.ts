import { Address, LucidEvolution, Network, SpendingValidator, validatorToAddress } from '@lucid-evolution/lucid';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { LucidError } from 'common/httpErrors';
import eitherify from 'utils/monad/either/eitherify/eitherify';
import valueOrError from 'utils/monad/either/patterns/valueOrError/valueOrError';
import { SpendingValidator as SpendingValidatorModel } from 'model/cardano/script/script.types';
import { Address as AddressModel } from 'model/cardano/cardano.types';
import { toScript } from '../../modelFactory/scriptFactory';
import { fromAddress } from '../../modelFactory/typesFactory';

const toAddress = (network: Network) =>
  (script: SpendingValidator): Address => validatorToAddress(network, script);

export default (lucid: LucidEvolution) => (validator: SpendingValidatorModel): Either<AppError, AddressModel> =>
  toScript(validator)
    .bind(eitherify(toAddress(lucid.config().network)))
    .bind(valueOrError<AddressModel>(new LucidError('Can not get Validator Address!')))
    .lift(fromAddress);
