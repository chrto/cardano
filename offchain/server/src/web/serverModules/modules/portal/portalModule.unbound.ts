import eitherify from 'utils/monad/either/eitherify/eitherify';
import { Router } from 'express';
import { Either } from 'tsmonad';
import { Fcn } from 'common/types';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { Context as PortalContext } from './context/context.types';
import { ModuleConfig, ModuleConfigFactory } from 'web/serverModules/types';
import { IAppLogger } from 'logger/appLogger.types';

export default (
  logger: IAppLogger,
  moduleDefinition: Fcn<[PluginSdkService], ModuleConfigFactory<PortalContext>>,
  moduleMiddlewares: Fcn<[AppConfig, PluginSdkService], ModuleConfigFactory<PortalContext>>,
  moduleParamHandler: Fcn<[PluginSdkService], ModuleConfigFactory<PortalContext>>,
  registerErrorHandlerMiddleware: ModuleConfigFactory<PortalContext>,
  registerRoutes: ModuleConfigFactory<PortalContext>
) =>
  (moduleConfig: ModuleConfig<PortalContext>) => {
    return (service: PluginSdkService, appConfig: AppConfig): Router =>
      Either.right<any, ModuleConfig<PortalContext>>(moduleConfig)
        .lift(moduleDefinition(service))
        .bind(eitherify<[ModuleConfig<PortalContext>], ModuleConfig<PortalContext>>(moduleMiddlewares(appConfig, service)))
        .bind(eitherify<[ModuleConfig<PortalContext>], ModuleConfig<PortalContext>>(moduleParamHandler(service)))
        .bind(eitherify<[ModuleConfig<PortalContext>], ModuleConfig<PortalContext>>(registerRoutes))
        .bind(eitherify(registerErrorHandlerMiddleware))
        .caseOf({
          right: (moduleConfig: ModuleConfig<PortalContext>): Router => moduleConfig.router,
          left: (error: any) => {
            logger.error('Error in Portal module loader..');
            throw error;
          }
        });
  };
