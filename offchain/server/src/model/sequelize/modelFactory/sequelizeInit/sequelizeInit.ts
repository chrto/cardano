import initUserModel from '../../model/user/user';
import initScriptModel from '../../model/script/scirpt';
import { InitModels } from './sequelizeInit.types';
import sequelizeInitUnbound from './sequelizeInit.unbound';

const initModel: InitModels = {
  userModel: initUserModel,
  scriptModel: initScriptModel
};

export default sequelizeInitUnbound(initModel);
