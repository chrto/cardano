import { Server } from 'http';
import { IAppLogger } from 'logger/appLogger.types';

export default (logger: IAppLogger, message: string) =>
  (server: Server) =>
    (resolve: () => void, reject: (reason: any) => void): void => {
      server.close((error: Error): void =>
        !!error
          ? reject(error)
          : (
            logger.info(message),
            resolve()
          ));
    };
