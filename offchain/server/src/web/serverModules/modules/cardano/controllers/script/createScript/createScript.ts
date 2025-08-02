import sanitizeModel from 'model/sequelize/sanitizeModel/sanitizeModel';
import scriptFactory from 'model/sequelize/model/script/factory/scriptFactory';
import createScriptUnbound from './createScript.unbound';
import bodyValidator from './validator/bodyValidator';

export default createScriptUnbound(bodyValidator, scriptFactory, sanitizeModel);
