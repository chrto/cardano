import { Address } from 'model/cardano/cardano.types';

export enum ModuleParams {
  address = 'address'
}
export interface RequestImplicits {
  address?: Address;
}
