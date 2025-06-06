import { Address, AddressDetails } from '@lucid-evolution/lucid';
import addressDetailsFactoty from '../../modelFactory/addressDetailsFactoty';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { LucidError } from 'common/httpErrors';
import valueOrError from 'utils/monad/either/patterns/valueOrError/valueOrError';
import { Address as AddressModel } from 'model/cardano/cardano.types';
import { AddressDetails as AddressDetailsModel } from 'model/cardano/addressDetails/addressDetails.types';
import { toAddress } from '../../modelFactory/typesFactory';
import { Fcn } from 'common/types';

export default (getAddressDetails: Fcn<[Address], AddressDetails>) => (address: AddressModel): Either<AppError, AddressDetailsModel> =>
  Either.right<AppError, AddressModel>(address)
    .bind(valueOrError<AddressModel>(new LucidError('Address is missing')))
    .lift<Address>(toAddress)
    .lift<AddressDetails>(getAddressDetails)
    .bind<AddressDetails>(valueOrError(new LucidError('Address details has not been received!')))
    .bind<AddressDetailsModel>(addressDetailsFactoty);
