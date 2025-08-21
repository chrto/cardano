import { Order, WhereOptions } from 'sequelize/types';
import { SequelizeService } from '../types';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import { ScriptReferenceRequired } from 'model/sequelize/model/scriptReference/scriptReference.types';

export interface ScriptReferenceService {
  getScriptReferenceById: SequelizeService<[string], ScriptReference>;
  getScriptReferences: SequelizeService<[WhereOptions?, Order?], ScriptReference[]>;
  createScriptReference: SequelizeService<[ScriptReferenceRequired], ScriptReference>;
}
