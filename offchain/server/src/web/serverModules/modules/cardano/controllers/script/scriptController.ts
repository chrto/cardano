import getAddress from './getAddress/getAddress';
import getScriptById from './getScriptById/getScriptById';
import getScripts from './getScripts/getScripts';
import createScript from './createScript/createScript';
import deleteScript from './deleteScript/deleteScript';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { ScriptController } from './scriptController.types';

export default ({ cardanoService, scriptService }: PluginSdkService): ScriptController =>
({
  getScirptAddress: getAddress(cardanoService),
  getScriptById,
  getScripts: getScripts(scriptService),
  createScript: createScript(scriptService),
  deleteScript: deleteScript(scriptService)
});
