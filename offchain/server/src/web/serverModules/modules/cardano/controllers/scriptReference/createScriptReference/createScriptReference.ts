import createScriptReferenceUnbound from './createScriptReference.unbound';
import bodyValidator from './validator/bodyValidator';
import scriptReferenceFactory from 'model/sequelize/model/scriptReference/factory/scriptReferenceFactory';
import sanitizeModel from 'model/sequelize/sanitizeModel/sanitizeModel';

export default createScriptReferenceUnbound(bodyValidator, scriptReferenceFactory, sanitizeModel);
