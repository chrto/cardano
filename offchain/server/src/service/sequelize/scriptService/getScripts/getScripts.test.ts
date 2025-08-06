import getScriptsUnbound from './getScripts.unbound';
import { AppError } from 'common/error';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { FindOptions, Options, Sequelize, WhereOptions } from 'sequelize';
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
const WHERE: WhereOptions = { firstName: 'Joe' };

describe('Service', () => {
  describe('Sequelize', () => {
    describe('Script Service', () => {
      describe(`Get scripts`, () => {
        let findAll: jest.Mock<Promise<Either<AppError, Script[]>>, [FindOptions]>;
        let scripts: Script[];
        let result: Either<AppError, Script[]>;

        beforeAll(async () => {
          initScriptModel(new Sequelize(SEQUELIZE_CONFIG));
          scripts = [Script.build(ITEMS)];
        });

        describe('Happy path', () => {
          beforeAll(async () => {
            findAll = jest.fn().mockResolvedValue(Either.right<AppError, Script[]>(scripts));
            result = await getScriptsUnbound
              .apply(null, [{ findAll }])
              .apply(null, [INCLUDES])
              .apply(null, [])
              .apply(null, [WHERE]);
          });

          it(`Should find scripts in storage and return Either with exact Script array in right side`, () => {
            result.do({
              right: (scripts: Script[]): void => {
                const scriptItems = scripts[0].get();
                expect(scripts).toBeArray;
                expect(scripts[0]).toBeInstanceOf(Script);
                expect(scriptItems).toStrictEqual({
                  ...ITEMS,
                  createdAt: scriptItems.createdAt,
                  updatedAt: scriptItems.updatedAt
                });
              },
              left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
            });
          });
        });

        describe('Error path', () => {
          const appError: NotFound = new NotFound('not exists');
          beforeAll(async () => {
            findAll = jest.fn().mockResolvedValue(Either.left<AppError, Script>(appError));
            result = await getScriptsUnbound
              .apply(null, [{ findAll }])
              .apply(null, [INCLUDES])
              .apply(null, [])
              .apply(null, [WHERE]);
          });

          it(`Should not find scripts in storage and return Either with exact error in left side`, () => {
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
