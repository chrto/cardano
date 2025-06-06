import addressParamHandlerUnbound from './addressParamHandler.unbound';
import addEntityInToRequestImplicits from 'web/serverModules/common/paramHandlers/addEntityInToRequestImplicits/addEntityInToRequestImplicits';
import handleError from 'web/serverModules/common/paramHandlers/handleError/handleError';

export default addressParamHandlerUnbound(addEntityInToRequestImplicits, handleError);
