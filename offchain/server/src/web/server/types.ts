import { Fcn } from 'common/types';
import { Express } from 'express';
import { Server } from 'http';
import { AsyncStartStop as AsyncStartStopSequelize } from 'model/sequelize/modelFactory/modelFactory.types';
import { IServerConfig } from './configuration/loader/server/serverConfig.types';

export type StartAction = Fcn<[], Promise<WebServer>>;
export type StopAction = Fcn<[], Promise<void>>;
export type RestartAction = Fcn<[], Promise<void>>;
export type ListeningAction = Fcn<[], boolean>;

interface AsyncStartStop {
  readonly start: StartAction;
  readonly stop: StopAction;
  readonly restart: RestartAction;
  readonly listening: ListeningAction;
}

export interface WebServer extends AsyncStartStop {
  readonly server: Server;
  readonly shutdownServer: Server;
  readonly sdkStartStop: AsyncStartStopSequelize;
  readonly config: IServerConfig;
  readonly expressApp: Express;
}
