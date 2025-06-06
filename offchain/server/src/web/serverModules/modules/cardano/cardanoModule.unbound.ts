import eitherify from 'utils/monad/either/eitherify/eitherify';
import { Router } from 'express';
import { Either } from 'tsmonad';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { Context as CardanoContext } from './context/context.types';
import { ModuleConfig, ModuleConfigFactory } from 'web/serverModules/types';
import { Fcn } from 'common/types';
import { IAppLogger } from 'logger/appLogger.types';

export default (
  logger: IAppLogger,
  moduleDefinition: Fcn<[PluginSdkService], ModuleConfigFactory<CardanoContext>>,
  moduleMiddlewares: Fcn<[AppConfig], ModuleConfigFactory<CardanoContext>>,
  moduleParamHandler: Fcn<[PluginSdkService], ModuleConfigFactory<CardanoContext>>,
  registerErrorHandlerMiddleware: ModuleConfigFactory<CardanoContext>,
  registerRoutes: ModuleConfigFactory<CardanoContext>
) =>
  (moduleConfig: ModuleConfig<CardanoContext>) => {
    return (service: PluginSdkService, appConfig: AppConfig): Router =>
      Either.right<any, ModuleConfig<CardanoContext>>(moduleConfig)
        .lift(moduleDefinition(service))
        .bind(eitherify<[ModuleConfig<CardanoContext>], ModuleConfig<CardanoContext>>(moduleMiddlewares(appConfig)))
        .bind(eitherify<[ModuleConfig<CardanoContext>], ModuleConfig<CardanoContext>>(moduleParamHandler(service)))
        .bind(eitherify<[ModuleConfig<CardanoContext>], ModuleConfig<CardanoContext>>(registerRoutes))
        .bind(eitherify(registerErrorHandlerMiddleware))
        .caseOf({
          right: (moduleConfig: ModuleConfig<CardanoContext>): Router => moduleConfig.router,
          left: (error: any) => {
            logger.error('Error in Cardano module loader..');
            throw error;
          }
        });
  };
