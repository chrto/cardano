import { AppError } from 'common/error';
import { Factory } from 'common/types';
import { Address, DatumHash } from 'model/cardano/cardano.types';
import { Either } from 'tsmonad';

// TODO add this into either utils as traverse fcn!
export const collectionFactory = <I, O> (factoryFcn: Factory<I, Either<AppError, O>>) => (collection: I[]): Either<AppError, O[]> => collection
  .map((i: I): Either<AppError, O> => factoryFcn(i))
  .reduce((acc: Either<AppError, O[]>, valueOrError: Either<AppError, O>): Either<AppError, O[]> =>
    acc.bind(values => valueOrError.caseOf({
      right: (value: O) => Either.right<AppError, O[]>([...values, value]),
      left: (appError: AppError) => Either.left<AppError, O[]>(appError)
    })), Either.right([]));

export const getUTxOsByAddressUri = (address: Address): string => `/matches/${address}`;
export const getDatumByHashUri = (datumHash: DatumHash): string => `/datums/${datumHash}`;
