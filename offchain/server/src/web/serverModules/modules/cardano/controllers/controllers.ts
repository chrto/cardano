import addressController from './address/addressController';
import scriptController from './script/scriptController';
import scriptReferenceController from './scriptReference/scriptReferenceController';
import transactionController from './transaction/transactionController';
import { CardanoModuleControllers } from './controllers.types';

const controllers: CardanoModuleControllers = { addressController, scriptController, scriptReferenceController, transactionController };
export default controllers;
