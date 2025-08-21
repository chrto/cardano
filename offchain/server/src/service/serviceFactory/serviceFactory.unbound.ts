import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { PluginSdkSequelize, SdkTransaction } from 'model/sequelize/modelFactory/modelFactory.types';
import { ISSOConfig } from 'web/server/configuration/loader/sso/ssoConfig.types';
import { Factory, Fcn } from 'common/types';

import sdkStartStopFactory from './sdkStartStop/sdkStartStop';
import sdkTransactionFactory from './sdkTransaction/sdkTransaction';
import { UserService } from '../sequelize/userService/userService.types';
import { ScriptService } from 'service/sequelize/scriptService/scriptService.types';
import { PluginSdkService } from './serviceFactory.types';
import { AuthenticationService } from '../http/authentication/types';
import { CardanoStorage } from 'storage/cardano/lucid/lucid.types';
import { CardanoService } from 'service/cardano/lucid/cardanoService.types';
import { ScriptReferenceService } from 'service/sequelize/scriptReferenceService/scriptReferenceService.types';
import { ICardanoNodeConfig } from 'web/server/configuration/loader/cardanoNode/cardanoNodeConfig.types';
import { CardanoKupoService } from 'service/http/kupo/kupoService.types';

const serviceFactoryUnbound = (
  authenticationServiceFactory: Factory<ISSOConfig, AuthenticationService>,
  userServiceFactory: Fcn<[], UserService>,
  scriptServiceFactory: Fcn<[SdkTransaction], ScriptService>,
  scriptReferenceServiceFactory: Fcn<[], ScriptReferenceService>,
  cardanoServiceFactory: Factory<CardanoStorage, CardanoService>,
  cardanoKupoServiceFactory: Factory<ICardanoNodeConfig, CardanoKupoService>
) =>
  (appConfig: AppConfig) =>
    (sdkSequelize: PluginSdkSequelize, cardanoStorage: CardanoStorage): PluginSdkService => {
      const sdkTransaction: SdkTransaction = sdkTransactionFactory(sdkSequelize);

      const authenticationService: AuthenticationService = authenticationServiceFactory(appConfig.sso);
      const userService: UserService = userServiceFactory();
      const scriptReferenceService: ScriptReferenceService = scriptReferenceServiceFactory();
      const scriptService: ScriptService = scriptServiceFactory(sdkTransaction);
      const cardanoService: CardanoService = cardanoServiceFactory(cardanoStorage);
      const cardanoKupoService: CardanoKupoService = cardanoKupoServiceFactory(appConfig.cardanoNode);
      return {
        sdkStartStop: sdkStartStopFactory(sdkSequelize),
        sdkTransaction: sdkTransaction,

        authenticationService,
        userService,
        scriptService,
        scriptReferenceService,
        cardanoService,
        cardanoKupoService
      };
    };

export default serviceFactoryUnbound;
