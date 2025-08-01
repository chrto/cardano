import sanitizeModel from 'model/sequelize/sanitizeModel/sanitizeModel';
import getScriptByIdUnbound from './getScriptById.unbound';

export default getScriptByIdUnbound(sanitizeModel);
