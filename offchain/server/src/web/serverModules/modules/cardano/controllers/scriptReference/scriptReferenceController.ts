import createScriptReference from './createScriptReference/createScriptReference';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { ScriptReferenceController } from './scriptReferenceController.types';

export default ({ scriptReferenceService }: PluginSdkService): ScriptReferenceController =>
({
  createScriptReference: createScriptReference(scriptReferenceService)
});
