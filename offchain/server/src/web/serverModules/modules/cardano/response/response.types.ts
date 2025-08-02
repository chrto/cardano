import { Credential } from 'model/cardano/credential/credential.types';
import { AddressDetails } from 'model/cardano/addressDetails/addressDetails.types';
import { OutputReference, TransactionOutput } from 'model/cardano/utxo/utxo.types';

import { Address, CborHex, TokenName, TxHash } from 'model/cardano/cardano.types';

export type AmountSerialized = string;
export type LovelaceSerialized = AmountSerialized;
export type AssetsSerialized = Record<TokenName | 'lovelace', AmountSerialized>;

export interface CredentialResponse extends Credential { }
export interface AddressDetailsResponse extends AddressDetails { }

export interface UTxOResponse extends OutputReference, TransactionOutput {
  assets: AssetsSerialized;
}

export interface TransactionCborHexResponse {
  cborHex: CborHex;
}

export interface TransactionHashResponse {
  txHash: TxHash;
}

export type AddressResponse = {
  address: Address;
};
