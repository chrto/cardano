import cardanoModuleUnbound from './cardanoModule.unbound';
import moduleDefinition from './config/moduleDefinition/moduleDefinition';
import moduleMiddlewares from './config/moduleMiddlewares/moduleMiddlewares';
import regiserRoutes from 'web/serverModules/configuration/routes/register/registerRoutes';
import moduleParamHandler from './config/moduleParamHandler/moduleParamHandler';
import registerErrorHandlerMiddleware from 'web/serverModules/configuration/middlewares/registerErrorHandlerMiddleware';
import contextFactory from './context/context';
import { Router } from 'express';
import appLogger from 'logger/appLogger';

export default cardanoModuleUnbound
  (
    appLogger,
    moduleDefinition,
    moduleMiddlewares,
    moduleParamHandler,
    registerErrorHandlerMiddleware,
    regiserRoutes
  )
  ({ router: Router(), contextFactory, moduleDefinition: null });
