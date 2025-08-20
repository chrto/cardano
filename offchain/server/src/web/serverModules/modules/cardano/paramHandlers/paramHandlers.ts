import { ParamHandlers } from 'web/serverModules/configuration/paramHandlers/moduleParamHandler.types';
import { ModuleParams, RequestImplicits } from './paramHandlers.types';
import addressParamHandler from './address/addressParamHandler';
import scriptParamHandler from './script/scriptParamHandler';
import scriptReferenceParamHandler from './scriptReference/scriptReferenceParamHandler';

const paramHandlers: ParamHandlers<ModuleParams, RequestImplicits> = {
  address: addressParamHandler,
  scriptId: scriptParamHandler,
  scriptReferenceId: scriptReferenceParamHandler
};
export default paramHandlers;
