import createScriptUnbound from './createScript.unbound';
import { AppError } from 'common/error';
import { PlutusVersion } from 'model/cardano/cardano.types';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { ScriptItems, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { CreateOptions, Options, Sequelize } from 'sequelize';
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
      describe(`Create new script`, () => {
        let createScriptModel: jest.Mock<Promise<Either<AppError, Script>>, [object, CreateOptions]>;
        let script: Script;
        beforeAll(async () => {
          initScriptModel(new Sequelize(SEQUELIZE_CONFIG));
          script = Script.build(ITEMS);
          createScriptModel = jest.fn().mockResolvedValue(Either.right<AppError, Script>(script));
          await createScriptUnbound
            .apply(null, [{ create: createScriptModel }])
            .apply(null, [])
            .apply(null, [ITEMS]);
        });

        it(`Should call create storage with exact parameters`, () => {
          expect(createScriptModel)
            .toHaveBeenCalledTimes(1);
          expect(createScriptModel)
            .toHaveBeenCalledWith(ITEMS, undefined);
        });
      });
    });
  });
});
