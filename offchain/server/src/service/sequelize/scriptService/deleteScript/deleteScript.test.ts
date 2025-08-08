import deleteScriptUnbound from './deleteScript.unbound';
import { AppError } from 'common/error';
import sequelizeInitUnbound from 'model/sequelize/modelFactory/sequelizeInit/sequelizeInit.unbound';
import initScriptReferenceModel from 'model/sequelize/model/scriptReference/scriptReference';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { DestroyOptions, Options, Sequelize, Transaction } from 'sequelize';
import { Either } from 'tsmonad';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { NotFound } from 'common/httpErrors';
import { ScriptItems, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';
import { SequelizeStorage } from 'storage/sequelize/factory/sequelizeStorage.types';
import { SdkTransaction } from 'model/sequelize/modelFactory/modelFactory.types';
import { SequelizeIncludes } from 'service/sequelize/types';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';

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
      as: 'scriptReferences'
    }
  ]
};

const TRANSACTION = {} as Transaction;

const SDK_TRANSACTION: SdkTransaction = {
  begin: () => Promise.resolve(TRANSACTION),
  commitOrRollback: (_tx: Transaction) => <T> (valOrErr: Either<AppError, T>) => Promise.resolve(valOrErr),
  rollback: (_tx: Transaction) => (err: AppError) => Promise.reject(err)
};

describe('Service', () => {
  describe('Sequelize', () => {
    describe('Script Service', () => {
      describe(`Delete script`, () => {
        let deleteScriptModel: jest.Mock<Promise<Either<AppError, number>>, [DestroyOptions]>;
        let initModel: jest.Mock<void, [Sequelize]>;
        let script: Script;
        let storage: SequelizeStorage<Script>;
        let result: Either<AppError, number>;

        beforeAll(async () => {
          initModel = jest.fn().mockImplementation(_ => null);
          sequelizeInitUnbound({
            scriptModel: initScriptModel,
            scriptReferenceModel: initScriptReferenceModel,
            userModel: initModel
          })(new Sequelize(SEQUELIZE_CONFIG));

          script = Script.build({ ...ITEMS, scriptReferences: [] }, { ...INCLUDES });
        });

        describe('Happy path', () => {
          beforeAll(async () => {
            deleteScriptModel = jest.fn().mockResolvedValue(Either.right<AppError, number>(1));

            storage = {
              findAll: null,
              findOne: null,
              findAndCountAll: null,
              findByPk: null,
              updateByPk: null,
              update: null,
              create: null,
              bulkCreate: null,
              destroy: deleteScriptModel
            };

            result = await deleteScriptUnbound
              (storage)
              (SDK_TRANSACTION)
              ({ transaction: TRANSACTION })
              (script);

          });

          it(`Should delete script from storage and return Either with count in right side`, () => {
            result.do({
              right: (count: number): void => {
                expect(count).toBe(1);
              },
              left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
            });
          });
        });

        describe('Error path', () => {
          const appError: NotFound = new NotFound('script not exists');
          beforeAll(async () => {
            deleteScriptModel = jest.fn().mockResolvedValue(Either.left<AppError, Script>(appError));
            result = await deleteScriptUnbound
              ({ ...storage, destroy: deleteScriptModel })
              (SDK_TRANSACTION)
              ({ transaction: TRANSACTION })
              (script);
          });

          it(`Should not delete script from storage and return Either with exact error in left side`, () => {
            result.do({
              right: (): void => fail(`Right side has not been expected`),
              left: (error: AppError) => {
                expect(error).toBeInstanceOf(NotFound);
                expect(error.message).toEqual(appError.message);
              }
            });
          });
        });
      });
    });
  });
});
