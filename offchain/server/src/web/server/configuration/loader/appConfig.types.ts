import { AppError } from 'common/error';
import { Either } from 'tsmonad';
import { IDatabaseConfig } from './database/databaseConfig.types';
import { ILoggerConfig } from './logger/loggerConfig.types';
import { INodeEnv } from './nodeEnv/nodeEnvConfig.types';
import { IServerConfig } from './server/serverConfig.types';
import { ISSOConfig } from './sso/ssoConfig.types';
import { ILucidConfig } from './lucid/lucidConfig.types';

export interface AppConfig extends INodeEnv {
  server: IServerConfig;
  database: IDatabaseConfig;
  sso: ISSOConfig;
  appLogger: ILoggerConfig;
  lucid: ILucidConfig;
}

export interface ConfigurationLoaders {
  loadNodeEnvConfiguration: AppConfigLoader<Either<AppError, AppConfig>>;
  loadServerConfiguration: AppConfigLoader<AppConfig>;
  loadDatabaseConfiguration: AppConfigLoader<AppConfig>;
  loadSSOConfiguration: AppConfigLoader<Either<AppError, AppConfig>>;
  loadLoggerConfiguration: AppConfigLoader<AppConfig>;
  loadLucidConfiguration: AppConfigLoader<Either<AppError, AppConfig>>;
}

export type AppConfigLoader<O> = (appConfig?: AppConfig) => O;
