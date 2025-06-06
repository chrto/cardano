import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { Express } from 'express';
import { Fcn } from 'common/types';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { PluginSdkSequelize } from 'model/sequelize/modelFactory/modelFactory.types';
import { IDatabaseConfig } from 'web/server/configuration/loader/database/databaseConfig.types';

import { ServerFactoryParams } from './factoryParams.types';
import { CardanoStorage } from 'storage/cardano/lucid/lucid.types';
import { ILucidConfig } from 'web/server/configuration/loader/lucid/lucidConfig.types';
import { AppError } from 'common/error';
import { Either } from 'tsmonad';
import lift from 'utils/monad/either/lift/lift';

export default (
  createExpressApp: Fcn<[], Express>,
  lucidStorageFactory: Fcn<[ILucidConfig], Promise<Either<AppError, CardanoStorage>>>,
  serviceFactory: Fcn<[AppConfig], Fcn<[PluginSdkSequelize, CardanoStorage], PluginSdkService>>,
  modelFactory: Fcn<[IDatabaseConfig], PluginSdkSequelize>
) =>
  (appConfig: AppConfig): Promise<Either<AppError, ServerFactoryParams>> =>
    lucidStorageFactory(appConfig.lucid)
      .then(lift((cardanoStorage: CardanoStorage) => ({
        expressApp: createExpressApp(),
        service: serviceFactory(appConfig)(modelFactory(appConfig.database), cardanoStorage),
        appConfig
      })));
