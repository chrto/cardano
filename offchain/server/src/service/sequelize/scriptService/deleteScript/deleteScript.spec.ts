import deleteScriptUnbound from './deleteScript.unbound';
import { AppError } from 'common/error';
import { PlutusVersion } from 'model/cardano/cardano.types';
import sequelizeInitUnbound from 'model/sequelize/modelFactory/sequelizeInit/sequelizeInit.unbound';
import initScriptReferenceModel, { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { ScriptItems, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { DestroyOptions, Options, Sequelize, Transaction } from 'sequelize';
import { SequelizeStorage } from 'storage/sequelize/factory/sequelizeStorage.types';
import { Either } from 'tsmonad';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { SequelizeIncludes } from 'service/sequelize/types';
import { SdkTransaction } from 'model/sequelize/modelFactory/modelFactory.types';

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

describe('Service', () => {
  describe('Sequelize', () => {
    describe('Script Service', () => {
      describe(`Delete script`, () => {
        let deleteScriptModel: jest.Mock<Promise<Either<AppError, number>>, [DestroyOptions]>;
        let initModel: jest.Mock<void, [Sequelize]>;
        let storage: SequelizeStorage<Script>;
        let script: Script;

        beforeAll(async () => {
          initModel = jest.fn().mockImplementation(_ => null);
          sequelizeInitUnbound({
            scriptModel: initScriptModel,
            scriptReferenceModel: initScriptReferenceModel,
            userModel: initModel
          })(new Sequelize(SEQUELIZE_CONFIG));

          script = Script.build({ ...ITEMS, references: [] }, { ...INCLUDES });
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

          await deleteScriptUnbound
            (storage)
            (SDK_TRANSACTION)
            ({ transaction: TRANSACTION })
            (script);
        });

        it(`Should call destroy storage with exact parameters`, () => {
          expect(deleteScriptModel)
            .toHaveBeenCalledTimes(1);
          expect(deleteScriptModel)
            .toHaveBeenCalledWith({ where: { id: ITEMS.id }, transaction: {} });
        });
      });
    });
  });
});
