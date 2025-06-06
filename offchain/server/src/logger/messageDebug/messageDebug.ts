import { IAppLogger } from '../appLogger.types';

export default function (message?: any, silent: boolean = false): IAppLogger {
  !silent && this.logger.debug(message);
  return this;
}
