import { AddressDetails, Credential } from '@lucid-evolution/lucid';
import { AddressDetails as AddressDetailsModel } from 'model/cardano/addressDetails/addressDetails.types';
import { Credential as CredentialModel } from 'model/cardano/credential/credential.types';

import { fromAddressType } from './typesFactory';
import { AppError } from 'common/error';
import { Either } from 'tsmonad';
import valueOrNothing from 'utils/monad/maybe/patterns/valueOrNothing/valueOrNothing';
import toMaybe from 'utils/monad/either/toMaybe/toMaybe';
import credentialFactory from './credentialFactory';

export const toUTxO = (addressDetails: AddressDetailsModel): AddressDetails => ({
  type: addressDetails.type,
  networkId: addressDetails.networkId,
  address: {
    bech32: addressDetails.address.bech32,
    hex: addressDetails.address.hex
  },
  paymentCredential: addressDetails.paymentCredential || null,
  stakeCredential: addressDetails.stakeCredential || null
});

const addCredential = (type: 'payment' | 'stake', credential: Credential) => (addressDetail: AddressDetailsModel): AddressDetailsModel =>
  valueOrNothing(credential)
    .lift(credentialFactory)
    .bind(toMaybe)
    .caseOf({
      just: (credentialModel: CredentialModel) => {
        switch (type) {
          case 'payment': return { ...addressDetail, paymentCredential: credentialModel };
          case 'stake': return { ...addressDetail, stakeCredential: credentialModel };
        }
      },
      nothing: () => addressDetail
    });

export default (addressDetails: AddressDetails): Either<AppError, AddressDetailsModel> =>
  fromAddressType(addressDetails.type)
    .lift<AddressDetailsModel>(type => ({
      type,
      networkId: addressDetails.networkId,
      address: {
        bech32: addressDetails.address.bech32,
        hex: addressDetails.address.hex
      }
    }))
    .lift(addCredential('payment', addressDetails.paymentCredential))
    .lift(addCredential('stake', addressDetails.stakeCredential));
