import getScriptsUnbound from './getScripts.unbound';
import { AppError } from 'common/error';
import scriptFactory from 'model/sequelize/model/script/factory/scriptFactory';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { Sequelize } from 'sequelize';
import { DEFAULT_DB_DIALECT } from 'src/defaults';
import { Either } from 'tsmonad';
import sanitizeModel from 'model/sequelize/sanitizeModel/sanitizeModel';
import { ScriptRequired, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';
import { ScriptService } from 'service/sequelize/scriptService/scriptService.types';

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
          describe('get scripts', () => {
            let sequelize: Sequelize;
            let scripts: Script[];
            let sanitizeEntities: jest.Mock<any[], [Script[]]>;
            let getScriptsExecutor: jest.Mock<Promise<Either<AppError, Script[]>>, [any, any, any]>;
            let scriptService: ScriptService;

            beforeAll(async () => {
              sequelize = new Sequelize(null, null, null, { dialect: DEFAULT_DB_DIALECT });
              initScriptModel(sequelize);
              scripts = [scriptFactory(SCRIPT_REQUIRED)
                .lift(scriptReq => Script.build(scriptReq))
                .caseOf({
                  right: (script) => script,
                  left: _ => null
                })];
              sanitizeEntities = jest.fn().mockImplementation((scripts: Script[]) => scripts.map(sanitizeModel));

              getScriptsExecutor = jest.fn().mockResolvedValue(Either.right(scripts));
              scriptService = {
                getScripts: jest.fn().mockImplementation(() => getScriptsExecutor),
                getScriptById: null,
                createScript: null
              };

              await getScriptsUnbound
                (sanitizeEntities)
                (scriptService)
                (null, null, null);
            });

            it(`Should call exact service`, () => {
              expect(scriptService.getScripts)
                .toHaveBeenCalledTimes(1);
              expect(scriptService.getScripts)
                .toHaveBeenCalledWith();

              expect(getScriptsExecutor)
                .toHaveBeenCalledTimes(1);
              expect(getScriptsExecutor)
                .toHaveBeenCalledWith();
            });

            it(`Should sanitize service result`, () => {
              expect(sanitizeEntities)
                .toHaveBeenCalledTimes(1);
              expect(sanitizeEntities)
                .toHaveBeenCalledWith(scripts);

              expect(sanitizeEntities)
                .toHaveBeenCalledAfter(getScriptsExecutor);
            });
          });
        });
      });
    });
  });
});
