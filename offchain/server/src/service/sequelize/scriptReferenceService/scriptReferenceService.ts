import { SequelizeIncludes } from '../types';
import { Script } from 'model/sequelize/model/script/scirpt';
import { ScriptReferenceService } from './scriptReferenceService.types';
import getScriptReferencById from './getScriptReferencById/getScriptReferencById';
import getScriptReferences from './getScriptReferences/getScriptReferences';
import createScriptReference from './createScriptReference/createScriptReference';

export default (): ScriptReferenceService => {
  const includes: SequelizeIncludes = {
    include: [
      {
        model: Script,
        as: 'script'
      }
    ]
  };

  return {
    getScriptReferenceById: getScriptReferencById(includes),
    getScriptReferences: getScriptReferences(includes),
    createScriptReference
  };
};
