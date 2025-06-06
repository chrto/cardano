import { Fcn } from 'common/types';
import { Express, Request, Response } from 'express';
import { IAppLogger } from 'logger/appLogger.types';

const middleware = (logger: IAppLogger, setTimeout: Fcn<[Fcn<[any[]], void>, number]>) =>
  (listening: Fcn<[], boolean>, actions: Fcn<[], Promise<void>>) =>
    (message: string) =>
      (_request: Request, response: Response<any>) => {
        if (listening()) {
          logger.debug(message);
          response.setHeader('Connection', 'close');
          response.send('ok');
          setTimeout(actions, 0);
        } else {
          logger.debug('server is not listening..');
          response
            .status(409)
            .send({ message: 'conflict', details: 'server is not listening..' });
        }
      };

export default (logger: IAppLogger, setTimeout: Fcn<[Fcn<[any[]], void>, number], NodeJS.Timeout>, cors: Fcn<[], any>) =>
  (expressApp: Express) =>
    (shutdown: Fcn<[], Promise<void>>, restart: Fcn<[], Promise<void>>, listening: Fcn<[], boolean>): Express =>
      expressApp
        .use(cors())
        .get('/shutdown', middleware
          (logger, setTimeout)
          (listening, shutdown)
          ('shuting down server..'))
        .get('/restart', middleware
          (logger, setTimeout)
          (listening, restart)
          ('restarting server..'));
