import stopUnbound from './stop.unbound';
import { exit } from 'process';
import logger from 'logger/appLogger';
import serverStopExecutor from '../../executors/serverStopExecutor/serverStopExecutor';
import shutdownServerStopExecutor from '../../executors/serverStopExecutor/shutdownServerStopExecutor';

export default stopUnbound(logger, shutdownServerStopExecutor, serverStopExecutor, setTimeout, clearTimeout, exit);
