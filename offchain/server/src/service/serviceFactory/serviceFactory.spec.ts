import { Factory, Fcn } from 'common/types';
import { ISSOConfig } from 'web/server/configuration/loader/sso/ssoConfig.types';

import pluginSdkServiceUnbound from './serviceFactory.unbound';
import { AuthenticationService } from '../http/authentication/types';
import { UserService } from '../sequelize/userService/userService.types';
import { PluginSdkService } from './serviceFactory.types';
import { CardanoService } from 'service/cardano/lucid/cardanoService.types';
import { CardanoStorage } from 'storage/cardano/lucid/lucid.types';
import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { PluginSdkSequelize } from 'model/sequelize/modelFactory/modelFactory.types';
import { ICardanoNodeConfig } from 'web/server/configuration/loader/cardanoNode/cardanoNodeConfig.types';
import { CardanoKupoService } from 'service/http/kupo/kupoService.types';

describe(`service`, () => {
  describe(`pluginSdkService module`, () => {
    let authenticationServiceFactory: Factory<ISSOConfig, AuthenticationService> = () => ({} as AuthenticationService);
    let userServiceFactory: Fcn<[], UserService> = () => ({} as UserService);
    let cardanoServiceFactory: Factory<CardanoStorage, CardanoService> = (_: CardanoStorage) => ({} as CardanoService);
    let cardanoKupoServiceFactory: Factory<ICardanoNodeConfig, CardanoKupoService> = (_: ICardanoNodeConfig) => ({} as CardanoKupoService);
    let pluginSdkService: PluginSdkService;

    beforeAll(() => {
      pluginSdkService = pluginSdkServiceUnbound
        (authenticationServiceFactory, userServiceFactory, cardanoServiceFactory, cardanoKupoServiceFactory)
        ({} as AppConfig)
        ({} as PluginSdkSequelize, {} as CardanoStorage);
    });

    beforeEach(() => {
      jest.clearAllMocks();
    });

    it(`Should return 'PluginSdkService'`, () => {
      expect(pluginSdkService).toBeObject;
      expect(Object.keys(pluginSdkService).length).toEqual(6);

      expect(pluginSdkService).toHaveProperty('sdkStartStop');
      expect(pluginSdkService).toHaveProperty('sdkTransaction');
      expect(pluginSdkService).toHaveProperty('authenticationService');
      expect(pluginSdkService).toHaveProperty('userService');
      expect(pluginSdkService).toHaveProperty('cardanoService');
      expect(pluginSdkService).toHaveProperty('cardanoKupoService');
    });
  });
});
