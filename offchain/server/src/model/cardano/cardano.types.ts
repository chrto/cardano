export type Amount = bigint;
export type Bech32 = string;
export type Hex = string;
export type CborHex = Hex;
export type PrivateKey = Bech32;
export type Lovelace = Amount;
export type Hash = string;
export type TxHash = Hash;
export type TxIndex = number;
export type TokenName = string;
export type DatumHash = Hash;
export type Datum = string;
export type KeyHash = string | PaymentKeyHash | StakeKeyHash;
export type PaymentKeyHash = Hash;
export type StakeKeyHash = Hash;
export type ScriptHash = Hash;
export type Assets = Record<TokenName | 'lovelace', Amount>;
export type Address = string;

export enum AddressType {
  Base = 'Base',
  Enterprise = 'Enterprise',
  Pointer = 'Pointer',
  Reward = 'Reward',
  Byron = 'Byron'
}

export enum CredentialType {
  Key = 'Key',
  Script = 'Script'
}

export enum PlutusVersion {
  PlutusV1 = 'PlutusV1',
  PlutusV2 = 'PlutusV2',
  PlutusV3 = 'PlutusV3'
}

export enum ScriptTypeExt {
  Native = 'Native'
}

export type ScriptType = PlutusVersion | ScriptTypeExt;

export enum Network {
  Mainnet = 'Mainnet',
  Preview = 'Preview',
  Preprod = 'Preprod',
  Custom = 'Custom'
}
