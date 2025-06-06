import { ParamHandlers } from 'web/serverModules/configuration/paramHandlers/moduleParamHandler.types';
import { ModuleParams, RequestImplicits } from './paramHandlers.types';
import addressParamHandler from './address/addressParamHandler';

const paramHandlers: ParamHandlers<ModuleParams, RequestImplicits> = { address: addressParamHandler };
export default paramHandlers;
