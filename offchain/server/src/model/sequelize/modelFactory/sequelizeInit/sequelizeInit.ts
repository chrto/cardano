import initUserModel from '../../model/user/user';
import initScriptModel from '../../model/script/scirpt';
import initScriptReferenceModel from '../../model/scriptReference/scriptReference';
import { InitModels } from './sequelizeInit.types';
import sequelizeInitUnbound from './sequelizeInit.unbound';

const initModel: InitModels = {
  userModel: initUserModel,
  scriptModel: initScriptModel,
  scriptReferenceModel: initScriptReferenceModel
};

export default sequelizeInitUnbound(initModel);
