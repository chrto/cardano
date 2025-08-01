import { Mandatory } from 'common/types';
import { CreationAttributes } from 'sequelize';
import { Script as ScriptSequelize } from './scirpt';
import { Script as ScriptCardano } from 'model/cardano/script/script.types';

export enum ScritpCategory {
  Gift = 'Gift',
  Burn = 'Burn',
  FortyTwo = 'FortyTwo',
  Vesting = 'Vesting',
  Unknown = 'Unknown'
}

export interface ScriptRequired extends ScriptCardano, CreationAttributes<ScriptSequelize> {
  category: ScritpCategory;
  title: string;
  description: string;
}

export interface ScriptItems extends Mandatory<ScriptRequired> {
  id: string;
}
