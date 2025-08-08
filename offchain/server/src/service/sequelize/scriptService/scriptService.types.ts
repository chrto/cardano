import { Script } from 'model/sequelize/model/script/scirpt';
import { ScriptItems } from 'model/sequelize/model/script/script.types';
import { Order, WhereOptions } from 'sequelize/types';
import { SequelizeService } from '../types';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import { ScriptReferenceItems } from 'model/sequelize/model/scriptReference/scriptReference.types';

export interface ScriptService {
  getScriptById: SequelizeService<[string], Script>;
  getScripts: SequelizeService<[WhereOptions?, Order?], Script[]>;
  createScript: SequelizeService<[ScriptItems], Script>;
  createScriptReference: SequelizeService<[Script, ScriptReferenceItems], ScriptReference>;
  deleteScript: SequelizeService<[Script], number>;
}
