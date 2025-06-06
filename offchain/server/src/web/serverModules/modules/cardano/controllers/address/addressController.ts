import getDetails from './getDetails/getDetails';
import getUTxOs from './getUTxOs/getUTxOs';
import { AddressController } from './addressController.types';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import getCredentialPayment from './getCredentialPayment/getCredentialPayment';

export default ({ cardanoService }: PluginSdkService): AddressController =>
({
  getUTxOs: getUTxOs(cardanoService),
  getDetails: getDetails(cardanoService),
  getCredentialPayment: getCredentialPayment(cardanoService)
});
