import { CreationAttributes } from 'sequelize';
import { Mandatory } from 'common/types';
import { ScriptReference as ScriptReferenceSequelize } from './scriptReference';
import { Address, TxHash, TxIndex } from 'model/cardano/cardano.types';

export interface ScriptReferenceRequired extends CreationAttributes<ScriptReferenceSequelize> {
  scriptId: string;
  address: Address;
  txId: TxHash;
  txIndex: TxIndex;
  unspend: boolean;
}

export interface ScriptReferenceItems extends Mandatory<ScriptReferenceRequired> {
  id: string;
}
