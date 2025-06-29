import 'source-map-support/register';

import lift from 'utils/monad/either/lift/lift';
import asyncLift from 'utils/monad/either/asyncLift/asyncLift';
import tap from 'utils/monad/either/tap/tap';
import fTap from 'utils/monad/either/tapFlatten/fTap';
import tapLeft from 'utils/monad/either/tapLeft/tapLeft';
import errorHandler from 'web/server/errorHandler/errorHandler';
import eitherify from 'utils/monad/either/eitherify/eitherify';
import loadAppConfig from './web/server/configuration/loader/appConfig';
import logAppConfig from './web/server/configuration/logger/logger';
import getServerParams from './web/server/factory/params/factoryParams';
import serverFactory from 'web/server/factory/server/serverFactory';
import registerModules from './web/server/registerModules/registerModules';
import { WebServer } from 'web/server/types';
import appLogger from 'logger/appLogger';
import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { ServerFactoryParams } from 'web/server/factory/params/factoryParams.types';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';

require('dotenv').config();

export const waitForShutdown = (server: WebServer): void => {
  process
    .on('SIGTERM', server.stop) // listen for TERM signal .e.g. kill
    .on('SIGINT', server.stop);  // listen for INT signal e.g. Ctrl-C
};

const startServer = (server: WebServer): Promise<WebServer> => server.start();

Promise.resolve(loadAppConfig())
  .then(tap<AppConfig>(appLogger.init))
  .then(tap<AppConfig>(logAppConfig))
  .then(asyncBind<AppConfig, ServerFactoryParams>(getServerParams))
  .then(fTap<ServerFactoryParams>(eitherify<[ServerFactoryParams], void>(registerModules)))
  .then(lift<ServerFactoryParams, WebServer>(serverFactory))
  .then(tap<WebServer>(appLogger.debug.bind(null, 'starting server...')))
  .then(asyncLift<WebServer, WebServer>(startServer))
  .then(tap<WebServer>(waitForShutdown))
  .then(tapLeft<WebServer>(errorHandler))
  .catch(errorHandler);
