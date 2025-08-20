import { ControllerFactory } from 'web/serverModules/types';
import { AddressController } from './address/addressController.types';
import { ScriptController } from './script/scriptController.types';
import { TransactionController } from './transaction/transactionController.types';
import { ScriptReferenceController } from './scriptReference/scriptReferenceController.types';

export interface CardanoModuleControllers {
  addressController: ControllerFactory<AddressController>;
  scriptController: ControllerFactory<ScriptController>;
  scriptReferenceController: ControllerFactory<ScriptReferenceController>;
  transactionController: ControllerFactory<TransactionController>;
}
