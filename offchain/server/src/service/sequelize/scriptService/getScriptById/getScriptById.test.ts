import getScriptByIdUnbound from './getScriptById.unbound';
import { AppError } from 'common/error';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { FindOptions, Options, Sequelize } from 'sequelize';
import { Either } from 'tsmonad';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { NotFound } from 'common/httpErrors';
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

describe('Service', () => {
  describe('Sequelize', () => {
    describe('Script Service', () => {
      describe(`Get Script by id`, () => {
        let findByPk: jest.Mock<Promise<Either<AppError, Script>>, [string, FindOptions, AppError]>;
        let script: Script;
        let result: Either<AppError, Script>;

        beforeAll(async () => {
          initScriptModel(new Sequelize(SEQUELIZE_CONFIG));
          script = Script.build(ITEMS);
        });

        describe('Happy path', () => {
          beforeAll(async () => {
            findByPk = jest.fn().mockResolvedValue(Either.right<AppError, Script>(script));
            result = await getScriptByIdUnbound
              .apply(null, [{ findByPk }])
              .apply(null, [INCLUDES])
              .apply(null, [])
              .apply(null, [ITEMS.id]);
          });

          it(`Should find script in storage and return Either with exact Script in right side`, () => {
            result.do({
              right: (script: Script): void => {
                expect(script).toBeInstanceOf(Script);
                expect(script.get()).toStrictEqual(ITEMS);
              },
              left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
            });
          });
        });

        describe('Error path', () => {
          const appError: NotFound = new NotFound('script not exists');
          beforeAll(async () => {
            findByPk = jest.fn().mockResolvedValue(Either.left<AppError, Script>(appError));
            result = await getScriptByIdUnbound
              .apply(null, [{ findByPk }])
              .apply(null, [INCLUDES])
              .apply(null, [])
              .apply(null, [ITEMS.id]);
          });

          it(`Should not find script in storage and return Either with exact error in left side`, () => {
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
