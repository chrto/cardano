import { Fcn } from 'common/types';
import { Express } from 'express';
import { Server } from 'http';
import { IAppLogger } from 'logger/appLogger.types';

export default (logger: IAppLogger) =>
  (port: number, infoMessage: string) =>
    (expressApp: Express) =>
      (resolve: Fcn<[Server], void>): void => {
        expressApp.listen(port, function (): void {
          logger.info(infoMessage);
          logger.debug(`on port ${port}`);
          resolve(this);
        });
      };
