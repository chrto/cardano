import { Address, Assets, Datum, DatumHash, TxHash, TxIndex } from '../cardano.types';
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
}

export interface UTxO extends OutputReference, TransactionOutput {
  assets: Assets;
}
