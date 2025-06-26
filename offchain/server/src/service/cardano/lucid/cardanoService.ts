import getAddressUTxOs from './getAddressUTxOs/getAddressUTxOs';
import getValidatorAddress from './getValidatorAddress/getValidatorAddress';
import { CardanoStorage } from 'storage/cardano/lucid/lucid.types';
import { CardanoService } from './cardanoService.types';
import getAddressDetail from './getAddressDetail/getAddressDetail';
import buildTransaction from './buildTransaction/buildTransaction';
import submitTransaction from './submitTransaction/submitTransaction';

export default (cardanoStorage: CardanoStorage): CardanoService => ({
  getAddressUTxOs: getAddressUTxOs(cardanoStorage),
  getAddressDetails: getAddressDetail(cardanoStorage),
  getValidatorAddress: getValidatorAddress(cardanoStorage),
  buildTransaction: buildTransaction(cardanoStorage),
  submitTransaction: submitTransaction(cardanoStorage)
});
