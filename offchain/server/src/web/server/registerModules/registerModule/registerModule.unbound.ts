import { pipe } from 'ramda';
import { Module, ModuleMapping } from '../registerModules.types';
import { ServerFactoryParams } from '../../factory/params/factoryParams.types';
import { Express } from 'express';
import { IAppLogger } from 'logger/appLogger.types';

const getModuleConfiguration = (moduleMapping: ModuleMapping) => (moduleName: string) => moduleMapping[moduleName];
const logDebugMessage = (logger: IAppLogger, moduleName: string) => (module: Module) => (logger.debug(`registering module ${moduleName} on '${module.routerPath}'`), module);
const register = ({ expressApp, service, appConfig }: ServerFactoryParams) => (module: Module): Express => expressApp.use(module.routerPath, module.routerConfigurator(service, appConfig));

export default (logger: IAppLogger) =>
  (params: ServerFactoryParams, moduleMapping: ModuleMapping) =>
    (moduleName: string) => {
      pipe(
        getModuleConfiguration(moduleMapping),
        logDebugMessage(logger, moduleName),
        register(params)
      )(moduleName);
    };
