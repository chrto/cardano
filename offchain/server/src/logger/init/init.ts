import initUnbound from './init.unbound';

import fileStorage from 'storage/file/file';
import { addColors } from 'winston';

import transports from '../config/transports/transports';
import loggerOptions from './options/options';
import buildLogger from './buildLogger/buildLogger';

export default initUnbound
  (fileStorage, true)
  (addColors)
  (transports, loggerOptions, buildLogger);
