import { Address, AddressType, CBORHex, Datum, PrivateKey, ScriptType } from '@lucid-evolution/lucid';
import {
  Address as AddressModel,
  ScriptType as ScriptTypeModel,
  CredentialType as CredentialTypeEnum,
  AddressType as AddressTypeEnum,
  PlutusVersion as PlutusVersionEnum,
  ScriptTypeExt as ScriptTypeExtEnum,
  CborHex as CborHexModel,
  PrivateKey as PrivateKeyModel,
  Datum as DatumModel
} from 'model/cardano/cardano.types';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { InvalidInput } from 'common/httpErrors';
import { Fcn } from 'common/types';

export const collectionFactory = <I, O> (factory: Fcn<[I], O>) => (input: I[]): O[] => input.map(factory);

// ScriptType
export const fromScriptType = (type: ScriptType): Either<AppError, ScriptTypeModel> => {
  switch (type) {
    case 'Native': return Either.right<AppError, ScriptTypeModel>(ScriptTypeExtEnum.Native);
    case 'PlutusV1': return Either.right<AppError, ScriptTypeModel>(PlutusVersionEnum.PlutusV1);
    case 'PlutusV2': return Either.right<AppError, ScriptTypeModel>(PlutusVersionEnum.PlutusV2);
    case 'PlutusV3': return Either.right<AppError, ScriptTypeModel>(PlutusVersionEnum.PlutusV3);
    default: return Either.left<AppError, ScriptTypeModel>(new InvalidInput(`Unknow Script Type '${type}'`));
  }
};

export const toScriptType = (type: ScriptTypeModel): Either<AppError, ScriptType> => {
  switch (type) {
    case ScriptTypeExtEnum.Native: return Either.right<AppError, ScriptType>('Native');
    case PlutusVersionEnum.PlutusV1: return Either.right<AppError, ScriptType>('PlutusV1');
    case PlutusVersionEnum.PlutusV2: return Either.right<AppError, ScriptType>('PlutusV2');
    case PlutusVersionEnum.PlutusV3: return Either.right<AppError, ScriptType>('PlutusV3');
    default: return Either.left<AppError, ScriptType>(new InvalidInput(`Unknow Script Type '${type}'`));
  }
};

// AddressType
export const fromAddressType = (type: AddressType): Either<AppError, AddressTypeEnum> => {
  switch (type) {
    case 'Base': return Either.right<AppError, AddressTypeEnum>(AddressTypeEnum.Base);
    case 'Enterprise': return Either.right<AppError, AddressTypeEnum>(AddressTypeEnum.Enterprise);
    case 'Pointer': return Either.right<AppError, AddressTypeEnum>(AddressTypeEnum.Pointer);
    case 'Reward': return Either.right<AppError, AddressTypeEnum>(AddressTypeEnum.Reward);
    case 'Byron': return Either.right<AppError, AddressTypeEnum>(AddressTypeEnum.Byron);
    default: return Either.left<AppError, AddressTypeEnum>(new InvalidInput(`Unknow Address Type '${type}'`));
  }
};
export const toAddressType = (type: AddressTypeEnum): Either<AppError, AddressType> => {
  switch (type) {
    case AddressTypeEnum.Base: Either.right<AppError, AddressType>('Base');
    case AddressTypeEnum.Enterprise: Either.right<AppError, AddressType>('Enterprise');
    case AddressTypeEnum.Pointer: Either.right<AppError, AddressType>('Pointer');
    case AddressTypeEnum.Reward: Either.right<AppError, AddressType>('Reward');
    case AddressTypeEnum.Byron: Either.right<AppError, AddressType>('Byron');
    default: return Either.left<AppError, AddressType>(new InvalidInput(`Unknow Address Type '${type}'`));
  }
};

// CredentialType
export const fromCredentialType = (type: 'Key' | 'Script'): Either<AppError, CredentialTypeEnum> => {
  switch (type) {
    case 'Key': return Either.right<AppError, CredentialTypeEnum>(CredentialTypeEnum.Key);
    case 'Script': return Either.right<AppError, CredentialTypeEnum>(CredentialTypeEnum.Script);
    default: return Either.left<AppError, CredentialTypeEnum>(new InvalidInput(`Unknow Credential Type '${type}'`));
  }
};

export const toCredentialType = (type: CredentialTypeEnum): Either<AppError, 'Key' | 'Script'> => {
  switch (type) {
    case CredentialTypeEnum.Key: return Either.right<AppError, 'Key' | 'Script'>('Key');
    case CredentialTypeEnum.Script: return Either.right<AppError, 'Key' | 'Script'>('Script');
    default: return Either.left<AppError, 'Key' | 'Script'>(new InvalidInput(`Unknow Credential Type '${type}'`));
  }
};

// // Lovelace
// export const fromLovelace = (lovelace: Lovelace): LovelaceModel => lovelace;
// export const toLovelace = (lovelace: LovelaceModel): Lovelace => lovelace;

// // TokenName
// export const fromTokenName = (name: Unit): TokenName => name;
// export const toTokenName = (name: TokenName): Unit => name;

// // Assets
// export const fromAssets = (assets: Assets): AssetsModel => Object.keys(assets).reduce((acc: AssetsModel, key: TokenName) => ({ ...acc, [key]: assets[key] }), {});
// export const toAssets = (assets: AssetsModel): Assets => Object.keys(assets).reduce((acc: Assets, key: Unit) => ({ ...acc, [key]: assets[key] }), {});

// Address
export const toAddress = (address: AddressModel): Address => address;
export const fromAddress = (address: Address): AddressModel => address;

// CBORHex
export const toCborHex = (cbor: CborHexModel): CBORHex => cbor;
export const fromCborHex = (cbor: CBORHex): CborHexModel => cbor;

// Bech32
export const toPrivateKey = (cbor: PrivateKeyModel): PrivateKey => cbor;
export const fromPrivateKey = (cbor: PrivateKey): PrivateKeyModel => cbor;

// Datum
export const toDatum = (cbor: DatumModel): Datum => cbor;
export const fromDatum = (cbor: Datum): DatumModel => cbor;
