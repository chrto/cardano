import { Script, ScriptType } from '@lucid-evolution/lucid';
import { Script as ScriptModel } from 'model/cardano/script/script.types';
import { fromScriptType, toScriptType } from './typesFactory';
import { ScriptType as ScriptTypeModel } from 'model/cardano/cardano.types';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';

export const toScript = (script: ScriptModel): Either<AppError, Script> =>
  toScriptType(script.type)
    .lift((type: ScriptType) => ({ type, script: script.script }));

export default (script: Script): Either<AppError, ScriptModel> =>
  fromScriptType(script.type)
    .lift((type: ScriptTypeModel) => ({ type, script: script.script }));
