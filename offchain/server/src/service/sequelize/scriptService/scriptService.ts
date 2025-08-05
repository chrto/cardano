import getScriptById from './getScriptById/getScriptById';
import getScripts from './getScripts/getScripts';
import createScript from './createScript/createScript';
import deleteScript from './deleteScript/deleteScript';
import { SequelizeIncludes } from '../types';

export default () => {
  const includes: SequelizeIncludes = {
    include: []
  };

  return {
    getScriptById: getScriptById(includes),
    getScripts: getScripts(includes),
    createScript,
    deleteScript
  };
};
