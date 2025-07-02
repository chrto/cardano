import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { PluginSdkSequelize } from 'model/sequelize/modelFactory/modelFactory.types';
import { ISSOConfig } from 'web/server/configuration/loader/sso/ssoConfig.types';
import { Factory, Fcn } from 'common/types';

import sdkStartStopFactory from './sdkStartStop/sdkStartStop';
import sdkTransactionFactory from './sdkTransaction/sdkTransaction';
import { UserService } from '../sequelize/userService/userService.types';
import { PluginSdkService } from './serviceFactory.types';
import { AuthenticationService } from '../http/authentication/types';
import { CardanoStorage } from 'storage/cardano/lucid/lucid.types';
import { CardanoService } from 'service/cardano/lucid/cardanoService.types';
import { ICardanoNodeConfig } from 'web/server/configuration/loader/cardanoNode/cardanoNodeConfig.types';
import { CardanoKupoService } from 'service/http/kupo/kupoService.types';

export default (
  authenticationServiceFactory: Factory<ISSOConfig, AuthenticationService>,
  userServiceFactory: Fcn<[], UserService>,
  cardanoServiceFactory: Factory<CardanoStorage, CardanoService>,
  cardanoKupoServiceFactory: Factory<ICardanoNodeConfig, CardanoKupoService>
) =>
  (appConfig: AppConfig) =>
    (sdkSequelize: PluginSdkSequelize, cardanoStorage: CardanoStorage): PluginSdkService => ({
      sdkStartStop: sdkStartStopFactory(sdkSequelize),
      sdkTransaction: sdkTransactionFactory(sdkSequelize),

      authenticationService: authenticationServiceFactory(appConfig.sso),
      userService: userServiceFactory(),
      cardanoService: cardanoServiceFactory(cardanoStorage),
      cardanoKupoService: cardanoKupoServiceFactory(appConfig.cardanoNode)
    });
