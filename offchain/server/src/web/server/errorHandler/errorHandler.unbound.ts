import { AppError } from 'common/error';
import { IAppLogger } from 'logger/appLogger.types';

const EXIT_WITH_CODE: number = -1;

export default (logger: IAppLogger, process: NodeJS.Process) =>
  (reason: AppError): never => {
    logger.error('*********************************************');
    logger.error('Unhandled exception during server initialization:');
    logger.error('*********************************************');
    logger.error(reason.code);
    logger.error(reason.message);
    logger.error('*********************************************');

    process.exit(EXIT_WITH_CODE);
  };
