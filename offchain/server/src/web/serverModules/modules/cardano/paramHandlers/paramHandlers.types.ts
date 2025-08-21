import { Address } from 'model/cardano/cardano.types';
import { Script } from 'model/sequelize/model/script/scirpt';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';

export enum ModuleParams {
  address = 'address',
  scriptId = 'scriptId',
  scriptReferenceId = 'scriptReferenceId'
}

export interface RequestImplicits {
  address?: Address;
  script?: Script;
  scriptReference?: ScriptReference;
}
