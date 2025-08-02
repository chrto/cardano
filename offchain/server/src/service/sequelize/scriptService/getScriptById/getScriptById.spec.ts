import getScriptByIdUnbound from './getScriptById.unbound';
import { AppError } from 'common/error';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { FindOptions, Options, Sequelize } from 'sequelize';
import { Either } from 'tsmonad';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { SequelizeIncludes } from 'service/sequelize/types';
import { NotFound } from 'common/httpErrors';
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

describe('Service', () => {
  describe('Sequelize', () => {
    describe('Script Service', () => {
      describe(`Get script by id`, () => {
        let findByPk: jest.Mock<Promise<Either<AppError, Script>>, [string, FindOptions, AppError]>;
        beforeAll(async () => {
          initScriptModel(new Sequelize(SEQUELIZE_CONFIG));
          findByPk = jest.fn().mockResolvedValue(Either.right<AppError, Script>(Script.build(ITEMS)));
          await getScriptByIdUnbound
            .apply(null, [{ findByPk }])
            .apply(null, [INCLUDES])
            .apply(null, [])
            .apply(null, [ITEMS.id]);
        });

        it(`Should call findByPk storage with exact parameters`, () => {
          expect(findByPk)
            .toHaveBeenCalledTimes(1);
          expect(findByPk)
            .toHaveBeenCalledWith(
              ITEMS.id,
              { ...INCLUDES },
              new NotFound(`Cannot find script identified by id = '${ITEMS.id}'`)
            );
        });
      });
    });
  });
});
