import { Address } from 'model/cardano/cardano.types';

export interface Query {
  scriptId?: string;
  address?: Address;
  unspend?: boolean;
}
