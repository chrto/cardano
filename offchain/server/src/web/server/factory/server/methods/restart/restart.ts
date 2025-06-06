import restartUnbound from './restart.unbound';
import { spawn } from 'child_process';

export default restartUnbound(process, spawn);
