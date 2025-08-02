import getScriptsUnbound from './getScripts.unbound';
import { AppError } from 'common/error';
import scriptFactory from 'model/sequelize/model/script/factory/scriptFactory';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { Sequelize, WhereOptions } from 'sequelize';
import { DEFAULT_DB_DIALECT } from 'src/defaults';
import { Either } from 'tsmonad';
import sanitizeModel from 'model/sequelize/sanitizeModel/sanitizeModel';
import { ScriptRequired, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';
import { ScriptService } from 'service/sequelize/scriptService/scriptService.types';
import { Query } from './getScripts.types';
import { Query as ExpressQuery } from 'express-serve-static-core';
import { AppRequest } from 'web/serverModules/types';
import { RequestImplicits } from '../../../paramHandlers/paramHandlers.types';

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
            let filterBuilder: jest.Mock<WhereOptions, [Query]>;
            let sanitizeEntities: jest.Mock<any[], [Script[]]>;
            let getScriptsExecutor: jest.Mock<Promise<Either<AppError, Script[]>>, [any, any, any]>;
            let scriptService: ScriptService;

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

              filterBuilder = jest.fn().mockReturnValue({ category: req.query.category });
              sanitizeEntities = jest.fn().mockImplementation((scripts: Script[]) => scripts.map(sanitizeModel));

              getScriptsExecutor = jest.fn().mockResolvedValue(Either.right(scripts));
              scriptService = {
                getScripts: jest.fn().mockImplementation(() => getScriptsExecutor),
                getScriptById: null,
                createScript: null
              };

              await getScriptsUnbound
                (sanitizeEntities, filterBuilder)
                (scriptService)
                (null, req, null);
            });

            it(`Should build filter from query`, () => {
              expect(filterBuilder)
                .toHaveBeenCalledTimes(1);
              expect(filterBuilder)
                .toHaveBeenCalledWith(req.query);
            });

            it(`Should call service with exact filter`, () => {
              expect(scriptService.getScripts)
                .toHaveBeenCalledTimes(1);
              expect(scriptService.getScripts)
                .toHaveBeenCalledWith();

              expect(getScriptsExecutor)
                .toHaveBeenCalledTimes(1);
              expect(getScriptsExecutor)
                .toHaveBeenCalledWith({ category: 'Gift' });

              expect(getScriptsExecutor)
                .toHaveBeenCalledAfter(filterBuilder);
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
