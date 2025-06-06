import { COLORS } from './config';

import { Maybe } from 'tsmonad';
import { Logger, LoggerOptions } from 'winston';
import { AbstractConfigSetColors } from 'winston/lib/winston/config';

import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { Fcn } from 'common/types';
import { IAppLogger } from '../appLogger.types';
import { FileStorage } from 'storage/file/file.types';
import { TransportsDefinition } from '../config/transports/transports.types';

export default (fileStorage: FileStorage, isMasterCluster: boolean) =>
  (addColors: Fcn<[AbstractConfigSetColors], any>) =>
    (
      transports: Fcn<[AppConfig], Maybe<TransportsDefinition>>,
      loggerOptions: Fcn<[TransportsDefinition], LoggerOptions>,
      buildLogger: Fcn<[LoggerOptions], Logger>
    ) =>
      function (appConfig: AppConfig): IAppLogger {
        Maybe.maybe(appConfig)
          .do({
            just: (appConfig: AppConfig): void => {
              if (isMasterCluster) {
                fileStorage.mkDirSync(appConfig.appLogger.dir, { recursive: true });
              }
            }
          })
          .do({
            just: (): void => {
              addColors(COLORS);
            }
          })
          .bind(transports)
          .lift(loggerOptions)
          .lift(buildLogger)
          .do({
            just: (logger: Logger): void => {
              this.logger = logger;
            }
          });
        return this;
      };
