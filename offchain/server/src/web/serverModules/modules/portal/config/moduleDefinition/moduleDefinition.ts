import moduleDefinitionUnbound from './moduleDefinition.unbound';

import authorization from 'web/serverModules/common/authorization/authorization';
import controllers from '../../controllers/controllers';

export default moduleDefinitionUnbound(controllers, authorization);
