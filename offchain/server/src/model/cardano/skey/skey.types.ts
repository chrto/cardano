import { Hex } from '../cardano.types';

export interface SKey {
  type: string;
  description: string;
  cborHex: Hex;
}