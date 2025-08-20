import getScriptReferencesUnbound from './getScriptReferences.unbound';
import filterBuilder from './filter/filterBuilder';
import sanitizeModels from 'model/sequelize/sanitizeModel/sanitizeModels';

export default getScriptReferencesUnbound(sanitizeModels, filterBuilder);
