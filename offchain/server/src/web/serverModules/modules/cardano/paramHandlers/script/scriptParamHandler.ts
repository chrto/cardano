import scriptParamHandlerUnbound from './scriptParamHandler.unbound';
import addEntityInToRequestImplicits from 'web/serverModules/common/paramHandlers/addEntityInToRequestImplicits/addEntityInToRequestImplicits';
import handleError from 'web/serverModules/common/paramHandlers/handleError/handleError';
import { isUuid } from 'utils/validation';

export default scriptParamHandlerUnbound(addEntityInToRequestImplicits, handleError, isUuid);
