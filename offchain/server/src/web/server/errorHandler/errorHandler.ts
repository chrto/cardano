import errorHandlerUnbound from './errorHandler.unbound';
import appLogger from 'logger/appLogger';

export default errorHandlerUnbound(appLogger, process);
