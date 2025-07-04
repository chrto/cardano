import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { AuthorizationHandlers } from 'web/serverModules/common/authorization/authorization.types';
import { ModuleConfig } from 'web/serverModules/types';
import { ModuleParams } from '../../paramHandlers/paramHandlers.types';
import { CardanoModuleControllers } from '../../controllers/controllers.types';
import { AddressController } from '../../controllers/address/addressController.types';
import { ScriptController } from '../../controllers/script/scriptController.types';
import scriptController from '../../controllers/script/scriptController';
import transactionController from '../../controllers/transaction/transactionController';
import { TransactionController } from '../../controllers/transaction/transactionController.types';
export default (
  { addressController }: CardanoModuleControllers,
  { allAuthenticated }: AuthorizationHandlers
) =>
  <CTX> (service: PluginSdkService) =>
    (moduleConfig: ModuleConfig<CTX>): ModuleConfig<CTX> => {
      const { getUTxOs, getDetails, getCredentialPayment }: AddressController = addressController(service);
      const { getScirptAddress }: ScriptController = scriptController(service);
      const { buildTransaction, submitTransaction }: TransactionController = transactionController(service);

      return {
        ...moduleConfig,
        moduleDefinition: {
          [`/:${ModuleParams.address}/utxos`]: { // TODO address will be better in query
            get: {
              action: getUTxOs,
              authorization: allAuthenticated
            }
          },
          [`/:${ModuleParams.address}/details`]: { // TODO address will be better in query
            get: {
              action: getDetails,
              authorization: allAuthenticated
            }
          },
          [`/:${ModuleParams.address}/credential/payment`]: { // TODO address will be better in query
            get: {
              action: getCredentialPayment,
              authorization: allAuthenticated
            }
          },
          [`/script/address`]: {
            get: {
              action: getScirptAddress,
              authorization: allAuthenticated
            }
          },
          [`/transaction/build`]: {
            post: {
              action: buildTransaction,
              authorization: allAuthenticated
            }
          },
          [`/transaction/submit`]: {
            post: {
              action: submitTransaction,
              authorization: allAuthenticated
            }
          }
        }
      };
    };
