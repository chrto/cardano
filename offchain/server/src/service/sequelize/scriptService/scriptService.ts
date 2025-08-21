import getScriptById from './getScriptById/getScriptById';
import getScripts from './getScripts/getScripts';
import createScript from './createScript/createScript';
import createScriptReference from './createScriptReference/createScriptReference';
import deleteScript from './deleteScript/deleteScript';
import { SequelizeIncludes } from '../types';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import { ScriptService } from './scriptService.types';
import { SdkTransaction } from 'model/sequelize/modelFactory/modelFactory.types';

const scriptService = (sdkTransaction: SdkTransaction): ScriptService => {
  const includes: SequelizeIncludes = {
    include: [
      {
        model: ScriptReference,
        as: 'scriptReferences'
      }
    ]
  };

  return {
    getScriptById: getScriptById(includes),
    getScripts: getScripts(includes),
    createScript,
    createScriptReference: createScriptReference(includes),
    deleteScript: deleteScript(sdkTransaction )
  };
};

export default scriptService;
