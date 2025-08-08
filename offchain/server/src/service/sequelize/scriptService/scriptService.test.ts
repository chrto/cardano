import scriptService from './scriptService';
import { ScriptService } from './scriptService.types';
import { SdkTransaction } from 'model/sequelize/modelFactory/modelFactory.types';
import { Transaction } from 'sequelize';
import { AppError } from 'common/error';
import { Either } from 'tsmonad';

const TRANSACTION = {} as Transaction;
const SDK_TRANSACTION: SdkTransaction = {
  begin: () => Promise.resolve(TRANSACTION),
  commitOrRollback: (_tx: Transaction) => <T> (valOrErr: Either<AppError, T>) => Promise.resolve(valOrErr),
  rollback: (_tx: Transaction) => (err: AppError) => Promise.reject(err)
};

describe('Service', () => {
  describe('Sequelize', () => {
    describe('Script Service', () => {
      let service: ScriptService;

      beforeAll(() => {
        service = scriptService(SDK_TRANSACTION);
      });

      it('Happy path', () => {
        expect(service).toBeInstanceOf(Object);
        expect(Object.keys(service)).toBeArrayOfSize(5);
        expect(service).toHaveProperty('getScriptById');
        expect(service).toHaveProperty('getScripts');
        expect(service).toHaveProperty('createScript');
        expect(service).toHaveProperty('createScriptReference');
        expect(service).toHaveProperty('deleteScript');
      });
    });
  });
});
