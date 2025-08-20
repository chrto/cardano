import moduleDefinitionUnbound from './moduleDefinition.unbound';
import { CardanoModuleControllers } from '../../controllers/controllers.types';
import { ModuleConfig } from 'web/serverModules/types';
import { Context as CardanoContext } from '../../context/context.types';
import { ScriptController } from '../../controllers/script/scriptController.types';
import { AuthorizationHandlers } from 'web/serverModules/common/authorization/authorization.types';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { ModuleDef } from 'web/serverModules/configuration/routes/register/registerRoutes.types';
import { AddressController } from '../../controllers/address/addressController.types';
import { TransactionController } from '../../controllers/transaction/transactionController.types';

const ADDRESS_CONTROLLER: AddressController = { getUTxOs: null, getDetails: null, getCredentialPayment: null };
const SCRIPT_CONTROLLER: ScriptController = { getScirptAddress: null, getScriptById: null, getScripts: null, createScript: null, deleteScript: null, addScriptReference: null };
const TRANSACTION_CONTROLLER: TransactionController = { buildTransaction: null, submitTransaction: null };

const AUTH_HANDLERS: AuthorizationHandlers = { allAuthenticated: null, isAdministrator: null };
const SERVICE: PluginSdkService = { sdkStartStop: null, sdkTransaction: null, authenticationService: null, userService: null, scriptService: null, scriptReferenceService: null, cardanoService: null, cardanoKupoService: null };
const MODULE_CONFIG: ModuleConfig<CardanoContext> = { moduleDefinition: null, router: null, contextFactory: null };

const EXPECTED_MODULE_DEFINITION: ModuleDef<CardanoContext> = {
  '/:address/utxos': {
    get: {
      action: ADDRESS_CONTROLLER.getUTxOs,
      authorization: AUTH_HANDLERS.allAuthenticated
    }
  },
  '/:address/details': {
    get: {
      action: ADDRESS_CONTROLLER.getDetails,
      authorization: AUTH_HANDLERS.allAuthenticated
    }
  },
  '/:address/credential/payment': {
    get: {
      action: ADDRESS_CONTROLLER.getCredentialPayment,
      authorization: AUTH_HANDLERS.allAuthenticated
    }
  },
  [`/script/address`]: {
    get: {
      action: SCRIPT_CONTROLLER.getScirptAddress,
      authorization: AUTH_HANDLERS.allAuthenticated
    }
  },

  [`/scripts`]: {
    post: {
      action: SCRIPT_CONTROLLER.createScript,
      authorization: AUTH_HANDLERS.allAuthenticated
    },
    get: {
      action: SCRIPT_CONTROLLER.getScripts,
      authorization: AUTH_HANDLERS.allAuthenticated
    }
  },
  [`/scripts/:scriptId`]: {
    get: {
      action: SCRIPT_CONTROLLER.getScriptById,
      authorization: AUTH_HANDLERS.allAuthenticated
    },
    delete: {
      action: SCRIPT_CONTROLLER.deleteScript,
      authorization: AUTH_HANDLERS.allAuthenticated
    }
  },

  [`/transaction/build`]: {
    post: {
      action: TRANSACTION_CONTROLLER.buildTransaction,
      authorization: AUTH_HANDLERS.allAuthenticated
    }
  },
  [`/transaction/submit`]: {
    post: {
      action: TRANSACTION_CONTROLLER.submitTransaction,
      authorization: AUTH_HANDLERS.allAuthenticated
    }
  }
};

describe('Modules', () => {
  describe('Cardano', () => {
    describe('Configuration', () => {
      describe('Module definition', () => {
        let controllers: CardanoModuleControllers = {} as CardanoModuleControllers;
        let result: ModuleConfig<CardanoContext>;

        beforeAll(() => {
          controllers.addressController = jest.fn().mockReturnValue(ADDRESS_CONTROLLER);
          controllers.scriptController = jest.fn().mockReturnValue(SCRIPT_CONTROLLER);
          controllers.transactionController = jest.fn().mockReturnValue(TRANSACTION_CONTROLLER);

          result = moduleDefinitionUnbound
            .apply(null, [controllers, AUTH_HANDLERS])
            .apply(null, [SERVICE])
            .apply(null, [MODULE_CONFIG]);
        });

        it(`Should build required controllers for cardano module`, () => {
          expect(controllers.addressController).toHaveBeenCalledTimes(1);
          expect(controllers.addressController).toHaveBeenCalledWith(SERVICE);

          expect(controllers.scriptController).toHaveBeenCalledTimes(1);
          expect(controllers.scriptController).toHaveBeenCalledWith(SERVICE);

          expect(controllers.transactionController).toHaveBeenCalledTimes(1);
          expect(controllers.transactionController).toHaveBeenCalledWith(SERVICE);
        });

        it(`Should set 'moduleDefinition' item in 'ModuleConfiguration' object, after controller has been builded`, () => {
          expect(result).toBeObject;
          expect(result).toHaveProperty('moduleDefinition');
          expect(result.moduleDefinition).toBeObject;
          expect(result.moduleDefinition).toStrictEqual(EXPECTED_MODULE_DEFINITION);
        });

        it(`Should keep rest of items`, () => {
          expect(result).toBeObject;
          expect(result).toHaveProperty('router');
          expect(result.router).toStrictEqual(MODULE_CONFIG.router);
          expect(result).toHaveProperty('contextFactory');
          expect(result.contextFactory).toStrictEqual(MODULE_CONFIG.contextFactory);
        });
      });
    });
  });
});
