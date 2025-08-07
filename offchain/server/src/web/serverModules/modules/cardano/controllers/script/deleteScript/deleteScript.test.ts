import deleteScriptUnbound from './deleteScript.unbound';
import appLogger from 'logger/appLogger';
import sequelizeInitUnbound from 'model/sequelize/modelFactory/sequelizeInit/sequelizeInit.unbound';
import initScriptReferenceModel from 'model/sequelize/model/scriptReference/scriptReference';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { Options, Sequelize, Transaction } from 'sequelize';
import { Context } from '../../../context/context.types';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { DeletedScript } from './deleteScript.types';
import { ScriptItems, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';
import scriptService from 'service/sequelize/scriptService/scriptService';
import { SdkTransaction } from 'model/sequelize/modelFactory/modelFactory.types';
import { SequelizeIncludes } from 'service/sequelize/types';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';

const SEQUELIZE_CONFIG: Options = {
  dialect: EDatabaseDialect.sqlite
};

const ITEMS: ScriptItems = {
  id: 'f923b2c9-ffcf-4a0a-bdc9-a4a4ae2a687e',
  type: PlutusVersion.PlutusV2,
  script: '49480100002221200101',
  category: ScritpCategory.Gift,
  title: 'PPP',
  description: 'Example of gift script from PPP'
};

const INCLUDES: SequelizeIncludes = {
  include: [
    {
      model: ScriptReference,
      as: 'references'
    }
  ]
};

const TRANSACTION = {} as Transaction;

const SDK_TRANSACTION: SdkTransaction = {
  begin: () => Promise.resolve(TRANSACTION),
  commitOrRollback: (_tx: Transaction) => <T> (valOrErr: Either<AppError, T>) => Promise.resolve(valOrErr),
  rollback: (_tx: Transaction) => (err: AppError) => Promise.reject(err)
};

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Cardano', () => {
      describe('controller', () => {
        describe('script controller', () => {
          describe('delete script by id', () => {
            let initModel: jest.Mock<void, [Sequelize]>;
            let script: Script;
            let context: Context;
            let result: Either<AppError, DeletedScript>;

            beforeAll(async () => {
              appLogger.error = (_) => appLogger; // disable logger
              initModel = jest.fn().mockImplementation(_ => null);
              sequelizeInitUnbound({
                scriptModel: initScriptModel,
                scriptReferenceModel: initScriptReferenceModel,
                userModel: initModel
              })(new Sequelize(SEQUELIZE_CONFIG));
              script = Script.build({ ...ITEMS, references: [] }, { ...INCLUDES });
            });

            describe('Happy path', () => {
              beforeAll(async () => {
                jest.clearAllMocks();
                Script.destroy = jest.fn().mockResolvedValue(1);
                context = { implicits: { script } };

                result = await deleteScriptUnbound
                  ()
                  (scriptService(SDK_TRANSACTION))
                  (context, null, null);
              });

              it('Should delete script from DB.', () => {
                expect(Script.destroy)
                  .toHaveBeenCalledTimes(1);
                expect(Script.destroy)
                  .toHaveBeenCalledWith({ where: { id: script.id }, transaction: TRANSACTION });
              });

              it(`Should return Either with exact object in right side`, () => {
                result.do({
                  right: (value: DeletedScript): void => {
                    expect(value)
                      .toEqual({ removedScriptId: ITEMS.id });
                  },
                  left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
                });
              });
            });

            describe('Errort path', () => {
              describe('Service', () => {
                const serviceError: AppError = new AppError('service.error', 'Internal server error');

                beforeAll(async () => {
                  jest.clearAllMocks();
                  Script.destroy = jest.fn().mockRejectedValue(serviceError);
                  context = { implicits: { script } };

                  result = await deleteScriptUnbound
                    ()
                    (scriptService(SDK_TRANSACTION))
                    (context, null, null);
                });

                it(`Should return Either with exact error in left side`, () => {
                  result.do({
                    right: (_value: DeletedScript): void => fail(`Right side has not been expected`),
                    left: (error: AppError) => {
                      expect(error)
                        .toBeInstanceOf(AppError);
                      expect(error.message)
                        .toEqual('Internal Server Error');
                    }
                  });
                });
              });
            });
          });
        });
      });
    });
  });
});
