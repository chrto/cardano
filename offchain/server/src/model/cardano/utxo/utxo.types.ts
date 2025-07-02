import { Address, Assets, Datum, DatumHash, ScriptHash, TxHash, TxIndex } from '../cardano.types';
import { Script } from '../script/script.types';

export interface OutputReference {
  txId: TxHash;
  txIndex: TxIndex;
}

export interface TransactionOutput {
  address: Address;
  datumHash?: DatumHash;
  datum?: Datum;
  scriptRef?: Script;
  scriptHash?: ScriptHash;
}

export interface UTxO extends OutputReference, TransactionOutput {
  assets: Assets;
}
