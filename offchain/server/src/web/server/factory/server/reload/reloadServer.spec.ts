import reloadServerUnbound from './reloadServer.unbound';
import { PluginSdkSequelize } from 'model/sequelize/modelFactory/modelFactory.types';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { IDatabaseConfig } from 'web/server/configuration/loader/database/databaseConfig.types';
import { WebServer } from 'web/server/types';
import { ServerFactoryParams } from '../../params/factoryParams.types';
import { CardanoStorage } from 'storage/cardano/lucid/lucid.types';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { ILucidConfig } from 'web/server/configuration/loader/lucid/lucidConfig.types';

const PLUGIN_SDK_SERVICE: PluginSdkService = {
  userService: {},
  authenticationService: {}
} as PluginSdkService;
const PLUGIN_SDK_SEQUELIZE: PluginSdkSequelize = {
  sequelize: {}
} as PluginSdkSequelize;
const WEB_SERVER: WebServer = {
  expressApp: {}
} as WebServer;
const FACTORY_PARAMS: ServerFactoryParams = {
  appConfig: {
    database: {
      url: 'url://to/db'
    }
  }
} as ServerFactoryParams;

const CARDANO_STORAGE: CardanoStorage = {} as CardanoStorage;

describe('Server Factory', () => {
  describe('reload server', () => {
    let lucidStorageFactory: jest.Mock<Promise<Either<AppError, CardanoStorage>>, [ILucidConfig]>;
    let serviceFactory: jest.Mock<PluginSdkService, [PluginSdkSequelize, CardanoStorage]>;
    let serviceFactoryUnbound: jest.Mock<jest.Mock<PluginSdkService, [PluginSdkSequelize, CardanoStorage]>, [AppConfig]>;
    let modelFactory: jest.Mock<PluginSdkSequelize, [IDatabaseConfig]>;
    let unregisterRouters: jest.Mock<WebServer, [WebServer]>;
    let registerModules: jest.Mock<ServerFactoryParams, [ServerFactoryParams]>;
    let webServerOrError: Either<AppError, WebServer>;

    beforeAll(async () => {
      lucidStorageFactory = jest.fn().mockResolvedValue(Either.right<AppError, CardanoStorage>(CARDANO_STORAGE));
      serviceFactory = jest.fn().mockReturnValue(PLUGIN_SDK_SERVICE);
      serviceFactoryUnbound = jest.fn().mockImplementation((_appConfig: AppConfig) => serviceFactory);
      modelFactory = jest.fn().mockReturnValue(PLUGIN_SDK_SEQUELIZE);
      unregisterRouters = jest.fn().mockImplementation((server: WebServer): WebServer => server);
      registerModules = jest.fn().mockImplementation((factoryParams: ServerFactoryParams): ServerFactoryParams => factoryParams);

      webServerOrError = await reloadServerUnbound
        (lucidStorageFactory, serviceFactoryUnbound, modelFactory, unregisterRouters, registerModules)
        (WEB_SERVER)
        (FACTORY_PARAMS);
    });

    it(`Should build model object on beginning`, () => {
      expect(modelFactory)
        .toHaveBeenCalledTimes(1);
      expect(modelFactory)
        .toHaveBeenCalledWith(FACTORY_PARAMS.appConfig.database);
    });

    it(`Should build service object, after model object and cardano storage has been builded`, () => {
      expect(serviceFactoryUnbound)
        .toHaveBeenCalledTimes(1);
      expect(serviceFactoryUnbound)
        .toHaveBeenCalledWith(FACTORY_PARAMS.appConfig);
      expect(serviceFactoryUnbound(FACTORY_PARAMS.appConfig))
        .toHaveBeenCalledWith(PLUGIN_SDK_SEQUELIZE, CARDANO_STORAGE);

      expect(serviceFactory)
        .toHaveBeenCalledAfter(modelFactory);

      expect(serviceFactory)
        .toHaveBeenCalledAfter(lucidStorageFactory);
    });

    it(`Should unregister routes, which had been registered, when server was started, or reloaded`, () => {
      expect(unregisterRouters)
        .toHaveBeenCalledTimes(1);
      expect(unregisterRouters)
        .toHaveBeenCalledWith(WEB_SERVER);
      expect(unregisterRouters)
        .toHaveBeenCalledAfter(serviceFactoryUnbound);
    });

    it(`Should register routes again, after routes has been unregistered`, () => {
      expect(registerModules)
        .toHaveBeenCalledTimes(1);
      expect(registerModules)
        .toHaveBeenCalledWith({
          ...FACTORY_PARAMS,
          expressApp: WEB_SERVER.expressApp,
          service: PLUGIN_SDK_SERVICE
        });
      expect(registerModules)
        .toHaveBeenCalledAfter(unregisterRouters);
    });

    it(`Should return webServer`, () => {
      webServerOrError.do({
        right: (webserver: WebServer) => expect(webserver).toStrictEqual(WEB_SERVER),
        left: (appError: AppError) => {
          throw new Error(`Left side has not been expected: ${appError.message}`);
        }
      });
    });
  });
});
