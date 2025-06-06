import { AppConfig } from 'web/server/configuration/loader/appConfig.types';

export interface IAppLogger {
  init: (config: AppConfig) => IAppLogger;
  error: (message: string) => IAppLogger;
  debug: (message: string, silent?: boolean) => IAppLogger;
  info: (message: string) => IAppLogger;
}
