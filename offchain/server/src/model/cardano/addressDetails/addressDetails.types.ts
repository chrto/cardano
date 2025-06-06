import { Address, AddressType, Hex } from '../cardano.types';
import { Credential } from '../credential/credential.types';
export interface AddressDetails {
  type: AddressType;
  networkId: number;
  address: {
    bech32: Address;
    hex: Hex;
  };
  paymentCredential?: Credential;
  stakeCredential?: Credential;
}
