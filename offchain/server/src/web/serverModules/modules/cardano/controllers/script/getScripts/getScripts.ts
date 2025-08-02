import filterBuilder from './filter/filterBuilder';
import getScriptsUnbound from './getScripts.unbound';
import sanitizeModels from 'model/sequelize/sanitizeModel/sanitizeModels';

export default getScriptsUnbound(sanitizeModels, filterBuilder);
