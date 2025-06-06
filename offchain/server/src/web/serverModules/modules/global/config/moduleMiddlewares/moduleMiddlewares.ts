import registerMiddlewares from 'web/serverModules/configuration/middlewares/registerMiddlewares';
import middlewares from 'web/serverModules/common/middlewares/middlewares';
import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { Context as GlobalContext } from '../../context/context.types';
import { ModuleConfigFactory } from 'web/serverModules/types';

const { cors, logger } = middlewares;
const { expressLogger, expressErrorLogger } = logger;

export default (appConfig: AppConfig): ModuleConfigFactory<GlobalContext> =>
  registerMiddlewares([
    cors,
    expressLogger(appConfig),
    expressErrorLogger(appConfig)
  ]);
