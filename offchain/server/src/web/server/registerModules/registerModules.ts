import registerModulesUnbound from './registerModules.unbound';
import serverModuleMapping from './registerModules.config';
import registerModule from './registerModule/registerModule';

export default registerModulesUnbound
  (registerModule)
  (serverModuleMapping);
