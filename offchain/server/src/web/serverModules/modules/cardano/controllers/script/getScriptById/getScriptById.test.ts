import getScriptByIdUnbound from './getScriptById.unbound';
import scriptFactory from 'model/sequelize/model/script/factory/scriptFactory';
import { AppError } from 'common/error';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { Sequelize } from 'sequelize';
import { DEFAULT_DB_DIALECT } from 'src/defaults';
import { Either } from 'tsmonad';
import { Context } from '../../../context/context.types';
import sanitizeModel from 'model/sequelize/sanitizeModel/sanitizeModel';
import { ScriptRequired, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';

const SCRIPT_REQUIRED: ScriptRequired = {
  type: PlutusVersion.PlutusV2,
  script: '49480100002221200101',
  category: ScritpCategory.Gift,
  title: 'PPP',
  description: 'Example of gift script from PPP'
};

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Portal', () => {
      describe('controller', () => {
        describe('script controller', () => {
          describe('get script by id', () => {
            let sequelize: Sequelize;
            let script: Script;
            let context: Context;
            let result: Either<AppError, Script>;

            beforeAll(async () => {
              sequelize = new Sequelize(null, null, null, { dialect: DEFAULT_DB_DIALECT });
              initScriptModel(sequelize);
              script = scriptFactory(SCRIPT_REQUIRED)
                .lift(scriptReq => Script.build(scriptReq))
                .caseOf({
                  right: (script: Script) => script,
                  left: _ => null
                });

              context = { implicits: { script } };
              result = await getScriptByIdUnbound
                .apply(null, [sanitizeModel])
                .apply(null, [context, null, null]);
            });

            it('Should return Either with script object in right side', () => {
              result.do({
                right: (script) => {
                  expect(script).toStrictEqual(context.implicits.script.get({ plain: true }));
                }
              });
            });
          });
        });
      });
    });
  });
});
