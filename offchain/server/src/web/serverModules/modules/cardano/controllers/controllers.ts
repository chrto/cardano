import addressController from './address/addressController';
import scriptController from './script/scriptController';
import transactionController from './transaction/transactionController';
import { CardanoModuleControllers } from './controllers.types';

const controllers: CardanoModuleControllers = { addressController, scriptController, transactionController };
export default controllers;
