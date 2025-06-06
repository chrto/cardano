import { Address, LucidEvolution, UTxO } from '@lucid-evolution/lucid';
import utxoFactory from '../../modelFactory/utxoFactory';
import { collectionFactory, toAddress } from '../../modelFactory/typesFactory';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { LucidError } from 'common/httpErrors';
import { Address as AddressModel } from 'model/cardano/cardano.types';
import { UTxO as UTxOModel } from 'model/cardano/utxo/utxo.types';
import valueOrDefault from 'utils/monad/either/patterns/valueOrDefault/valueOrDefault';
import bind from 'utils/monad/either/bind/bind';
import valueOrError from 'utils/monad/either/patterns/valueOrError/valueOrError';
import asyncLift from 'utils/monad/either/asyncLift/asyncLift';
import lift from 'utils/monad/either/lift/lift';

export default (lucid: LucidEvolution) => (address: AddressModel): Promise<Either<AppError, UTxOModel[]>> =>
  Promise.resolve(valueOrError<AddressModel>(new LucidError('Address is missing'))(address))
    .then(lift<AddressModel, Address>(toAddress))
    .then(asyncLift<Address, UTxO[]>(lucid.utxosAt))
    .then(bind(valueOrDefault<UTxO[]>([])))
    .then(lift(collectionFactory<UTxO, UTxOModel>(utxoFactory)))
    .catch((err) => Either.left<AppError, UTxOModel[]>(new LucidError(err.message)));
