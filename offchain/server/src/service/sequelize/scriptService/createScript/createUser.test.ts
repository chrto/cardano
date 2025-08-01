import createScriptUnbound from './createScript.unbound';
import { AppError } from 'common/error';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { CreateOptions, Options, Sequelize } from 'sequelize';
import { Either } from 'tsmonad';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { Conflict } from 'common/httpErrors';
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

describe('Service', () => {
  describe('Sequelize', () => {
    describe('Script Service', () => {
      describe(`Create new script`, () => {
        let createScriptModel: jest.Mock<Promise<Either<AppError, Script>>, [object, CreateOptions]>;
        let result: Either<AppError, Script>;

        beforeAll(async () => {
          initScriptModel(new Sequelize(SEQUELIZE_CONFIG));
        });

        describe('Happy path', () => {
          beforeAll(async () => {
            createScriptModel = jest.fn().mockResolvedValue(Either.right<AppError, Script>(Script.build(ITEMS)));
            result = await createScriptUnbound
              .apply(null, [{ create: createScriptModel }])
              .apply(null, [])
              .apply(null, [ITEMS]);
          });

          it(`Should create new script in storage and return Either with Script model in right side`, () => {
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
          const appError: Conflict = new Conflict('script exists');
          beforeAll(async () => {
            createScriptModel = jest.fn().mockResolvedValue(Either.left<AppError, Script>(appError));
            result = await createScriptUnbound
              .apply(null, [{ create: createScriptModel }])
              .apply(null, [])
              .apply(null, [ITEMS]);
          });

          it(`Should not create new script in storage and return Either with exact error in left side`, () => {
            result.do({
              right: (): void => fail(`Right side has not been expected`),
              left: (error: AppError) => {
                expect(error).toBeInstanceOf(Conflict);
                expect(error.message).toEqual(appError.message);
              }
            });
          });
        });
      });
    });
  });
});
