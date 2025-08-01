import getScriptsUnbound from './getScripts.unbound';
import { AppError } from 'common/error';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { FindOptions, Options, Sequelize, WhereOptions } from 'sequelize';
import { Either } from 'tsmonad';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { SequelizeIncludes } from 'service/sequelize/types';
import { ScriptItems, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';

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

const INCLUDES: SequelizeIncludes = { include: [] };

const WHERE: WhereOptions = { title: 'PPP' };

describe('Service', () => {
  describe('Sequelize', () => {
    describe('Scripts Service', () => {
      describe(`Get scripts`, () => {
        let findAll: jest.Mock<Promise<Either<AppError, Script[]>>, [FindOptions]>;
        beforeAll(async () => {
          initScriptModel(new Sequelize(SEQUELIZE_CONFIG));
          findAll = jest.fn().mockResolvedValue(Either.right<AppError, Script[]>([Script.build(ITEMS)]));
          await getScriptsUnbound
            .apply(null, [{ findAll }])
            .apply(null, [INCLUDES])
            .apply(null, [])
            .apply(null, [WHERE]);
        });

        it(`Should call findAll storage with exact parameters`, () => {
          expect(findAll)
            .toHaveBeenCalledTimes(1);
          expect(findAll)
            .toHaveBeenCalledWith(
              { where: WHERE, order: undefined, ...INCLUDES }
            );
        });
      });
    });
  });
});
