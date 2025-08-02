import { Script } from 'model/sequelize/model/script/scirpt';
import { ScriptRequired } from 'model/sequelize/model/script/script.types';
import { Order, WhereOptions } from 'sequelize/types';
import { SequelizeService } from '../types';

export interface ScriptService {
  getScriptById: SequelizeService<[string], Script>;
  getScripts: SequelizeService<[WhereOptions?, Order?], Script[]>;
  createScript: SequelizeService<[ScriptRequired], Script>;
}
