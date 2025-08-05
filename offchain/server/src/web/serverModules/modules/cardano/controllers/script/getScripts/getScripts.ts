import getScriptsUnbound from './getScripts.unbound';
import filterBuilder from './filter/filterBuilder';
import sanitizeModels from 'model/sequelize/sanitizeModel/sanitizeModels';

export default getScriptsUnbound(sanitizeModels, filterBuilder);
