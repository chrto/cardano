import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { TransactionController } from './transactionController.types';
import buildTransaction from './buildTransaction/buildTransaction';
import submitTransaction from './submitTransaction/submitTransaction';

export default ({ cardanoService }: PluginSdkService): TransactionController =>
({
  buildTransaction: buildTransaction(cardanoService),
  submitTransaction: submitTransaction(cardanoService)
});
