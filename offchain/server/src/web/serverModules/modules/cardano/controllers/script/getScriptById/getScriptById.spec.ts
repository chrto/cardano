import getScriptByIdUnbound from './getScriptById.unbound';
import scriptFactory from 'model/sequelize/model/script/factory/scriptFactory';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { Model, Sequelize } from 'sequelize';
import { DEFAULT_DB_DIALECT } from 'src/defaults';
import { Context } from '../../../context/context.types';
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
    describe('Cardano', () => {
      describe('controller', () => {
        describe('script controller', () => {
          describe('get script by id', () => {
            let sequelize: Sequelize;
            let sanitizeEntity: jest.Mock<any, [Model<Script>]>;
            let script: Script;
            let context: Context;

            beforeAll(async () => {
              sequelize = new Sequelize(null, null, null, { dialect: DEFAULT_DB_DIALECT });
              initScriptModel(sequelize);
              script = scriptFactory(SCRIPT_REQUIRED)
                .lift(scriptReq => Script.build(scriptReq))
                .caseOf({
                  right: (script) => script,
                  left: _ => null
                });

              context = { implicits: { script } };
              sanitizeEntity = jest.fn().mockImplementation((script: Script) => script.get({ plain: true }));
              await getScriptByIdUnbound
                .apply(null, [sanitizeEntity])
                .apply(null, [context, null, null]);
            });

            it('Should sanitize script model from context implicits', () => {
              expect(sanitizeEntity)
                .toHaveBeenCalledTimes(1);
              expect(sanitizeEntity)
                .toHaveBeenCalledWith(context.implicits.script);
            });
          });
        });
      });
    });
  });
});
