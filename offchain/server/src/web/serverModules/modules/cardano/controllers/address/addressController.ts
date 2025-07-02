import { AddressController } from './addressController.types';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import getDetails from './getDetails/getDetails';
import getUTxOs from './getUTxOs/getUTxOs';
import getCredentialPayment from './getCredentialPayment/getCredentialPayment';

export default ({ cardanoService, cardanoKupoService }: PluginSdkService): AddressController =>
({
  getUTxOs: getUTxOs(cardanoKupoService),
  getDetails: getDetails(cardanoService),
  getCredentialPayment: getCredentialPayment(cardanoService)
});
