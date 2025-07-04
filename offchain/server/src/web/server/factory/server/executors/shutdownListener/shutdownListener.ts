import shutdownListenerUnbound from './shutdownListener.unbound';
import * as express from 'express';
import * as cors from 'cors';
import appLogger from 'logger/appLogger';

export default shutdownListenerUnbound
  (appLogger, setTimeout, cors)
  (express());
