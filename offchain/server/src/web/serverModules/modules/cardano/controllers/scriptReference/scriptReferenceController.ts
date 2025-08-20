import getScriptReferenceById from './getScriptReferenceById/getScriptReferenceById';
import createScriptReference from './createScriptReference/createScriptReference';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { ScriptReferenceController } from './scriptReferenceController.types';

export default ({ scriptReferenceService }: PluginSdkService): ScriptReferenceController =>
({
  getScriptReferenceById: getScriptReferenceById,
  createScriptReference: createScriptReference(scriptReferenceService)
});
