import { Fcn } from 'common/types';
import { ISSOConfig } from 'web/server/configuration/loader/sso/ssoConfig.types';

import pluginSdkServiceUnbound from './serviceFactory.unbound';
import { AuthenticationService } from '../http/authentication/types';
import { UserService } from '../sequelize/userService/userService.types';
import { PluginSdkService } from './serviceFactory.types';
import { CardanoService } from 'service/cardano/cardanoService.types';
import { CardanoStorage } from 'storage/cardano/lucid/lucid.types';
import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { PluginSdkSequelize } from 'model/sequelize/modelFactory/modelFactory.types';

describe(`service`, () => {
  describe(`pluginSdkService module`, () => {
    let authenticationServiceFactory: Fcn<[ISSOConfig], AuthenticationService> = () => ({} as AuthenticationService);
    let userServiceFactory: Fcn<[], UserService> = () => ({} as UserService);
    let cardanoServiceFactory: Fcn<[CardanoStorage], CardanoService> = (_: CardanoStorage) => ({} as CardanoService);
    let pluginSdkService: PluginSdkService;

    beforeAll(() => {
      pluginSdkService = pluginSdkServiceUnbound
        .apply(null, [authenticationServiceFactory, userServiceFactory, cardanoServiceFactory])
        .apply(null, [{} as AppConfig])
        .apply(null, [{} as PluginSdkSequelize, {} as CardanoStorage]);
    });

    beforeEach(() => {
      jest.clearAllMocks();
    });

    it(`Should return 'PluginSdkService'`, () => {
      expect(pluginSdkService).toBeObject;

      expect(pluginSdkService).toHaveProperty('sdkStartStop');
      expect(pluginSdkService).toHaveProperty('sdkTransaction');
      expect(pluginSdkService).toHaveProperty('authenticationService');
      expect(pluginSdkService).toHaveProperty('userService');
      expect(pluginSdkService).toHaveProperty('cardanoService');
    });
  });
});
