import getScriptsUnbound from './getScripts.unbound';
import filterBuilder from './filter/filterBuilder';
import sanitizeModels from 'model/sequelize/sanitizeModel/sanitizeModels';
import scriptFactory from 'model/sequelize/model/script/factory/scriptFactory';
import { AppError } from 'common/error';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { Sequelize } from 'sequelize';
import { DEFAULT_DB_DIALECT } from 'src/defaults';
import { Either } from 'tsmonad';
import sanitizeModel from 'model/sequelize/sanitizeModel/sanitizeModel';
import { ScriptRequired, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';
import { AppRequest } from 'web/serverModules/types';
import { RequestImplicits } from '../../../paramHandlers/paramHandlers.types';
import { Query } from './getScripts.types';
import { Query as ExpressQuery } from 'express-serve-static-core';
import { ScriptService } from 'service/sequelize/scriptService/scriptService.types';

type AppReq = AppRequest<unknown, RequestImplicits, Query & ExpressQuery>;

const SCRIPT_REQUIRED_GIFT: ScriptRequired = {
  type: PlutusVersion.PlutusV2,
  script: '49480100002221200101',
  category: ScritpCategory.Gift,
  title: 'PPP',
  description: 'Example of gift script from PPP'
};

const SCRIPT_REQUIRED_BURN: ScriptRequired = {
  type: PlutusVersion.PlutusV2,
  script: '581f581d01000022232632498cd5ce24810b6974206275726e7321212100120011',
  category: ScritpCategory.Burn,
  title: 'PPP',
  description: 'Example of gift script from PPP'
};

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Cardano', () => {
      describe('controller', () => {
        describe('script controller', () => {
          describe('get scripts', () => {
            let req: AppReq;
            let sequelize: Sequelize;
            let scripts: Script[];
            let getScriptsExecutor: jest.Mock<Promise<Either<AppError, Script[]>>, [any, any, any]>;
            let scriptService: ScriptService;
            let result: Either<AppError, Script[]>;

            beforeAll(async () => {

              req = {
                query: {
                  category: ScritpCategory.Gift
                }
              } as AppReq;

              sequelize = new Sequelize(null, null, null, { dialect: DEFAULT_DB_DIALECT });
              initScriptModel(sequelize);
              scripts = [SCRIPT_REQUIRED_GIFT, SCRIPT_REQUIRED_BURN].map(script =>
                scriptFactory(script)
                  .lift(scriptReq => Script.build(scriptReq))
                  .caseOf({
                    right: (script) => script,
                    left: _ => null
                  })
              );

              getScriptsExecutor = jest.fn().mockResolvedValue(Either.right(scripts.filter(script => script.category === req.query.category)));
              scriptService = {
                getScripts: jest.fn().mockImplementation(() => getScriptsExecutor),
                getScriptById: null,
                createScript: null,
                deleteScript: null
              };

              result = await getScriptsUnbound
                (sanitizeModels, filterBuilder)
                (scriptService)
                (null, req, null);
            });

            it('Should return Either with exact script objects in right side', () => {
              const expected = scripts.filter(script => script.category === ScritpCategory.Gift).map(sanitizeModel);

              result.do({
                right: (scriptsResult) => {
                  expect(scriptsResult).toBeArray;
                  expect(scriptsResult).toBeArrayOfSize(1);
                  expect(scriptsResult).toStrictEqual(expected);
                }
              });
            });
          });
        });
      });
    });
  });
});
