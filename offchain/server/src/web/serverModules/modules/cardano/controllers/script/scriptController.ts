import getAddress from './getAddress/getAddress';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { ScriptController } from './scriptController.types';

export default ({ cardanoService }: PluginSdkService): ScriptController =>
({
  getScirptAddress: getAddress(cardanoService)
});
