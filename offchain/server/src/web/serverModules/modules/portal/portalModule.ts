import portalModuleUnbound from './portalModule.unbound';
import appLogger from 'logger/appLogger';
import moduleDefinition from './config/moduleDefinition/moduleDefinition';
import moduleMiddlewares from './config/moduleMiddlewares/moduleMiddlewares';
import moduleParamHandler from './config/moduleParamHandler/moduleParamHandler';
import registerErrorHandlerMiddleware from 'web/serverModules/configuration/middlewares/registerErrorHandlerMiddleware';
import regiserRoutes from 'web/serverModules/configuration/routes/register/registerRoutes';
import contextFactory from './context/context';
import { Router } from 'express';

export default portalModuleUnbound
  (
    appLogger,
    moduleDefinition,
    moduleMiddlewares,
    moduleParamHandler,
    registerErrorHandlerMiddleware,
    regiserRoutes
  )
  ({ router: Router(), contextFactory, moduleDefinition: null });
