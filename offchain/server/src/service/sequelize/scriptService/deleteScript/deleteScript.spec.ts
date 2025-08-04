import deleteScriptUnbound from './deleteScript.unbound';
import { AppError } from 'common/error';
import { PlutusVersion } from 'model/cardano/cardano.types';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { ScriptItems, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { DestroyOptions, Options, Sequelize } from 'sequelize';
import { SequelizeStorage } from 'storage/sequelize/factory/sequelizeStorage.types';
import { Either } from 'tsmonad';
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

describe('Service', () => {
  describe('Sequelize', () => {
    describe('Script Service', () => {
      describe(`Delete script`, () => {
        let deleteScriptModel: jest.Mock<Promise<Either<AppError, number>>, [DestroyOptions]>;
        let storage: SequelizeStorage<Script>;

        beforeAll(async () => {
          initScriptModel(new Sequelize(SEQUELIZE_CONFIG));
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
            ()
            (Script.build(ITEMS));
        });

        it(`Should call destroy storage with exact parameters`, () => {
          expect(deleteScriptModel)
            .toHaveBeenCalledTimes(1);
          expect(deleteScriptModel)
            .toHaveBeenCalledWith({ where: { id: ITEMS.id } });
        });
      });
    });
  });
});
