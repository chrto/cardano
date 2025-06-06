import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { IDatabaseConfig } from 'web/server/configuration/loader/database/databaseConfig.types';
import { ENodeENV } from 'web/server/configuration/loader/nodeEnv/nodeEnvConfig.types';
import { IServerConfig } from 'web/server/configuration/loader/server/serverConfig.types';
import { ISSOConfig } from 'web/server/configuration/loader/sso/ssoConfig.types';
import { Express } from 'express';
import { Fcn } from 'common/types';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { PluginSdkSequelize } from 'model/sequelize/modelFactory/modelFactory.types';

import factoryParamsUnbound from './factoryParams.unbound';
import { ServerFactoryParams } from './factoryParams.types';
import { ILoggerConfig } from 'web/server/configuration/loader/logger/loggerConfig.types';
import { ILucidConfig } from 'web/server/configuration/loader/lucid/lucidConfig.types';
import { CardanoStorage } from 'storage/cardano/lucid/lucid.types';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
// import { LucidEvolution, Network, Provider } from '@lucid-evolution/lucid';
// import { Maybe } from 'tsmonad';

const APP_CONFIG: AppConfig = {
  environment: ENodeENV.development,
  server: {} as IServerConfig,
  database: {} as IDatabaseConfig,
  sso: {} as ISSOConfig,
  appLogger: {} as ILoggerConfig,
  lucid: {} as ILucidConfig
};

describe('server factory params module', () => {
  describe(`'factoryParams'`, () => {
    const expressApp: Express = {} as Express;
    const sdkSequelize: PluginSdkSequelize = {} as PluginSdkSequelize;
    const service: PluginSdkService = {} as PluginSdkService;
    const cardanoStorage: CardanoStorage = {} as CardanoStorage;

    let createExpressApp: Fcn<[], Express>;
    let lucidStorageFactory: Fcn<[ILucidConfig], Promise<Either<AppError, CardanoStorage>>>;
    let serviceFactory: jest.Mock<PluginSdkService, [PluginSdkSequelize, CardanoStorage]>;
    let serviceFactoryUnbound: jest.Mock<jest.Mock<PluginSdkService, [PluginSdkSequelize, CardanoStorage]>, [AppConfig]>;
    let modelFactory: Fcn<[IDatabaseConfig], PluginSdkSequelize>;

    let factoryParams: Fcn<[AppConfig], Promise<Either<AppError, ServerFactoryParams>>>;

    beforeAll(() => {
      createExpressApp = jest.fn().mockReturnValue(expressApp);
      lucidStorageFactory = jest.fn().mockResolvedValue(Either.right<AppError, CardanoStorage>(cardanoStorage));
      serviceFactory = jest.fn().mockReturnValue(service);
      serviceFactoryUnbound = jest.fn().mockImplementation((_appConfig: AppConfig) => serviceFactory);
      modelFactory = jest.fn().mockReturnValue(sdkSequelize);

      factoryParams = factoryParamsUnbound
        .apply(null, [
          createExpressApp,
          lucidStorageFactory,
          serviceFactoryUnbound,
          modelFactory
        ]);
    });
    it(`Should return params for server factory`, async () => {
      const expected: ServerFactoryParams = {
        service: service,
        expressApp: expressApp,
        appConfig: APP_CONFIG
      };

      const paramsOrError = await factoryParams(APP_CONFIG);
      paramsOrError.do({
        right: (params: ServerFactoryParams) => {
          expect(params).toBeObject;
          expect(params).toStrictEqual(expected);
        },
        left: (appError: AppError) => {
          throw new Error(`Left side has not been expected: ${appError.message}`);
        }
      });
    });
  });
});
