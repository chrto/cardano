import { AppError } from 'common/error';
import eitherify from 'utils/monad/either/eitherify/eitherify';
import { Either } from 'tsmonad';
import { Address, Assets, CborHex, Lovelace, TokenName, TxHash } from 'model/cardano/cardano.types';
import { AddressResponse, AssetsSerialized, LovelaceSerialized, TransactionCborHexResponse, TransactionHashResponse, UTxOResponse } from './response.types';
import { UTxO } from 'model/cardano/utxo/utxo.types';
import { Fcn } from 'common/types';

export const collectionSerializer = <I, O> (serializer: Fcn<[I], O>) => (collection: I[]): O[] => collection.map(serializer);

// Assets
export const assetsSerializer = (assets: Assets): AssetsSerialized => Object.keys(assets).reduce((acc: AssetsSerialized, key: TokenName) => ({ ...acc, [key]: assets[key].toString() }), {});
export const assetsDeserializer = (assets: AssetsSerialized): Assets => Object.keys(assets).reduce((acc: Assets, key: TokenName) => ({ ...acc, [key]: BigInt(assets[key]) }), {}); // TODO - Should be Either

// UTxO
export const utxoSerializer = (utxo: UTxO): UTxOResponse => ({
  ...utxo,
  assets: assetsSerializer(utxo.assets)
});

// Lovelace
export const lovelaceSerializer = (amount: Lovelace): LovelaceSerialized => amount.toString();
export const lovelaceDeserializer = (amount: LovelaceSerialized): Either<AppError, Lovelace> => eitherify(BigInt)(amount);

// Address
export const addressSerializer = (address: Address): AddressResponse => ({ address });

// TransactionCborHexResponse
export const transactionCborHexSerializer = (cborHex: CborHex): TransactionCborHexResponse => ({ cborHex });

// TransactionHashResponse
export const transactionHashSerializer = (txHash: TxHash): TransactionHashResponse => ({ txHash });
