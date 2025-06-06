import { Fcn } from 'common/types';
import { Server } from 'http';
import { IAppLogger } from 'logger/appLogger.types';

export default (
  logger: IAppLogger,
  shutdownServerStopExecutor: Fcn<[Server], Fcn<[Fcn<[], void>, Fcn<[any], void>], void>>,
  serverStopExecutor: Fcn<[Server], Fcn<[Fcn<[], void>, Fcn<[any], void>], void>>,
  setTimeout: Fcn<[Fcn<[], void>, number], NodeJS.Timeout>,
  clearTimeout: Fcn<[NodeJS.Timeout], void>,
  exit: Fcn<[], never>
) =>
  function (): Promise<void> {
    const shutdwonTimeout: NodeJS.Timeout = setTimeout(() => {
      logger.debug('shutdown timeout reached - exiting application');
      exit();
    }, this.config.shutdownTimeout);

    return this.sdkStartStop.stop()
      .then((): Promise<void[]> => Promise.all([
        new Promise<void>(shutdownServerStopExecutor(this.shutdownServer)),
        new Promise<void>(serverStopExecutor(this.server))
      ]))
      .then((): void => clearTimeout(shutdwonTimeout))
      .catch((reason: any): void => {
        clearTimeout(shutdwonTimeout);
        logger.debug('Can not close server listeners!')
          .debug('exiting application...')
          .debug('reason:')
          .debug(reason);
        exit();
      });
  };
