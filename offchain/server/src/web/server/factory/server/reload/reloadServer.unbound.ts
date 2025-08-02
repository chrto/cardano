import { WebServer } from '../../../types';
import { ServerFactoryParams } from '../../params/factoryParams.types';
import { Fcn } from 'common/types';
import { PluginSdkSequelize } from 'model/sequelize/modelFactory/modelFactory.types';
import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { IDatabaseConfig } from 'web/server/configuration/loader/database/databaseConfig.types';
import { CardanoStorage } from 'storage/cardano/lucid/lucid.types';
import { ILucidConfig } from 'web/server/configuration/loader/lucid/lucidConfig.types';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import lift from 'utils/monad/either/lift/lift';
import doer from 'utils/monad/either/do/doer';

export default (
  lucidStorageFactory: Fcn<[ILucidConfig], Promise<Either<AppError, CardanoStorage>>>,
  serviceFactory: Fcn<[AppConfig], Fcn<[PluginSdkSequelize, CardanoStorage], PluginSdkService>>,
  modelFactory: Fcn<[IDatabaseConfig], PluginSdkSequelize>,
  unregisterRouters: Fcn<[WebServer], WebServer>,
  registerModules: Fcn<[ServerFactoryParams], void>
) =>
  (server?: WebServer) =>
    (params: ServerFactoryParams): Promise<Either<AppError, WebServer>> =>
      lucidStorageFactory(params.appConfig.lucid)
        .then(lift((cardanoStorage) => serviceFactory(params.appConfig)(modelFactory(params.appConfig.database), cardanoStorage)))
        .then(lift((service: PluginSdkService): ServerFactoryParams => ({
          ...params,
          service
        })))
        .then(doer({
          right: (_: ServerFactoryParams) => unregisterRouters(server)
        }))
        .then(doer({
          right: (serverFactoryParams: ServerFactoryParams) => registerModules({ ...serverFactoryParams, expressApp: server.expressApp })
        }))
        .then(lift(_ => server));
