import { Address, Datum } from 'model/cardano/cardano.types';
import { LovelaceSerialized } from '../../../response/response.types';

export interface Body {
  walletAddress: Address;
  contractAddress: Address;
  datum: Datum;
  amount: LovelaceSerialized;
}

export interface Query {

}
