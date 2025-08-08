import createScriptUnbound from './createScript.unbound';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { Response } from 'express';
import { Query as ExpressQuery } from 'express-serve-static-core';
import { Sequelize } from 'sequelize';
import { DEFAULT_DB_DIALECT } from 'src/defaults';
import { ScriptBody } from './createScript.types';
import { AppError } from 'common/error';
import { Either } from 'tsmonad';
import { AppRequest } from 'web/serverModules/types';
import { RequestImplicits } from '../../../paramHandlers/paramHandlers.types';
import { ScriptService } from 'service/sequelize/scriptService/scriptService.types';
import { ScriptItems, ScriptRequired, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';

type BodyValidator = jest.Mock<Either<AppError, ScriptBody>, [ScriptBody]>;
type AppReq = AppRequest<unknown, RequestImplicits, ExpressQuery, ScriptBody>;
const UUID: string = '92b814ed-1aff-46c1-b669-0c9fd2ea81a3';

const SCRIPT_REQUIRED: ScriptRequired = {
  type: PlutusVersion.PlutusV2,
  script: '49480100002221200101',
  category: ScritpCategory.Gift,
  title: 'PPP',
  description: 'Example of gift script from PPP'
};

const BODY: ScriptBody = SCRIPT_REQUIRED;

const SCRIPT_ITEMS: ScriptItems = {
  id: UUID,
  ...SCRIPT_REQUIRED
};

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Caredano', () => {
      describe('controller', () => {
        describe('script controller', () => {
          describe('create script', () => {
            let res: Response;
            let req: AppReq;
            let bodyValidator: BodyValidator;
            let scriptFactory: jest.Mock<Either<AppError, ScriptItems>, [ScriptRequired]>;
            let sequelize: Sequelize;
            let script: Script;
            let sanitizeEntity: jest.Mock<any, [Script]>;
            let scriptService: ScriptService;
            let createScriptExecutor: jest.Mock<Promise<Either<AppError, Script>>, [ScriptRequired]>;

            beforeAll(async () => {
              req = {
                body: BODY
              } as AppReq;

              res = {
                status: jest.fn().mockReturnThis()
              } as unknown as Response;
              sequelize = new Sequelize(null, null, null, { dialect: DEFAULT_DB_DIALECT });
              initScriptModel(sequelize);

              script = Script.build(SCRIPT_ITEMS);

              bodyValidator = jest.fn().mockImplementation((body: ScriptBody) => Either.right(body));
              sanitizeEntity = jest.fn().mockImplementation((script: Script) => script.get({ plain: true }));
              createScriptExecutor = jest.fn().mockResolvedValue(Either.right(script));
              scriptFactory = jest.fn().mockReturnValue(Either.right(SCRIPT_ITEMS));
              scriptService = {
                createScript: jest.fn().mockImplementation(() => createScriptExecutor),
                createScriptReference: null,
                deleteScript: null,
                getScripts: null,
                getScriptById: null
              };

              await createScriptUnbound
                (bodyValidator, scriptFactory, sanitizeEntity)
                (scriptService)
                (null, req, res);
            });

            it('Should validate body as first', () => {
              expect(bodyValidator)
                .toHaveBeenCalledTimes(1);
              expect(bodyValidator)
                .toHaveBeenCalledWith(req.body);
            });

            it('Should calculate default script items, after body has been validated', async () => {
              expect(scriptFactory)
                .toHaveBeenCalledTimes(1);
              expect(scriptFactory)
                .toHaveBeenCalledWith(req.body);

              expect(scriptFactory)
                .toHaveBeenCalledAfter(bodyValidator);
            });

            it('Should create new script in DB (call service), after validation has been passed', async () => {
              expect(scriptService.createScript)
                .toHaveBeenCalledTimes(1);
              expect(scriptService.createScript)
                .toHaveBeenCalledWith();

              expect(createScriptExecutor)
                .toHaveBeenCalledTimes(1);
              expect(createScriptExecutor)
                .toHaveBeenCalledWith(SCRIPT_ITEMS);

              expect(createScriptExecutor)
                .toHaveBeenCalledAfter(scriptFactory);
            });

            it('Should sanitize response and set response status to code 201', async () => {
              expect(res.status)
                .toHaveBeenCalledTimes(1);
              expect(res.status)
                .toHaveBeenCalledWith(201);

              expect(sanitizeEntity)
                .toHaveBeenCalledTimes(1);
              expect(sanitizeEntity)
                .toHaveBeenCalledWith(script);
            });
          });
        });
      });
    });
  });
});
