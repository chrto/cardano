import getAddressUTxOs from './getAddressUTxOs/getAddressUTxOs';
import getDatum from './getDatum/getDatum';
import { CardanoKupoService } from './kupoService.types';
import { ICardanoNodeConfig } from 'web/server/configuration/loader/cardanoNode/cardanoNodeConfig.types';

export default (cardanoNodeConfig: ICardanoNodeConfig): CardanoKupoService => ({
  getAddressUTxOs: getAddressUTxOs(cardanoNodeConfig),
  getDatumByHash: getDatum(cardanoNodeConfig)
});
