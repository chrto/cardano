import { Address } from 'model/cardano/cardano.types';
import { Script } from 'model/sequelize/model/script/scirpt';

export enum ModuleParams {
  address = 'address',
  scriptId = 'scriptId'
}

export interface RequestImplicits {
  address?: Address;
  script?: Script;
}
