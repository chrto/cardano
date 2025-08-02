import { ParamHandlers } from 'web/serverModules/configuration/paramHandlers/moduleParamHandler.types';
import { ModuleParams, RequestImplicits } from './paramHandlers.types';
import addressParamHandler from './address/addressParamHandler';
import scriptParamHandler from './script/scriptParamHandler';

const paramHandlers: ParamHandlers<ModuleParams, RequestImplicits> = {
  address: addressParamHandler,
  scriptId: scriptParamHandler
};
export default paramHandlers;
