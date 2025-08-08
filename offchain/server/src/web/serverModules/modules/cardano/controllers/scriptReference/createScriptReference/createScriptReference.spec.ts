import createScriptReferenceUnbound from './createScriptReference.unbound';
import initScriptReferenceModel, { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import initScriptModel from 'model/sequelize/model/script/scirpt';
import sequelizeInitUnbound from 'model/sequelize/modelFactory/sequelizeInit/sequelizeInit.unbound';
import { Response } from 'express';
import { Query as ExpressQuery } from 'express-serve-static-core';
import { Options, Sequelize } from 'sequelize';
import { AppError } from 'common/error';
import { Either } from 'tsmonad';
import { AppRequest } from 'web/serverModules/types';
import { RequestImplicits } from '../../../paramHandlers/paramHandlers.types';
import { ScriptReferenceBody } from './createScriptReference.types';
import { ScriptReferenceItems, ScriptReferenceRequired } from 'model/sequelize/model/scriptReference/scriptReference.types';
import { ScriptReferenceService } from 'service/sequelize/scriptReferenceService/scriptReferenceService.types';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';

type BodyValidator = jest.Mock<Either<AppError, ScriptReferenceBody>, [ScriptReferenceBody]>;
type AppReq = AppRequest<unknown, RequestImplicits, ExpressQuery, ScriptReferenceBody>;
const UUID: string = '92b814ed-1aff-46c1-b669-0c9fd2ea81a3';

const SEQUELIZE_CONFIG: Options = {
  dialect: EDatabaseDialect.sqlite
};

const SCRIPT_REFERENCE_REQUIRED: ScriptReferenceRequired = {
  scriptId: 'f0962fc9-882d-416d-bc08-fed1d5aa3a36',
  address: 'addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla',
  txId: '82e75104c2ffcab389fae6a9c87ebbe99e83cd7826d02534e77783b12c62e467',
  txIndex: 0
};

const BODY: ScriptReferenceBody = SCRIPT_REFERENCE_REQUIRED;

const SCRIPT_REFERENCE_ITEMS: ScriptReferenceItems = {
  id: UUID,
  unspend: true,
  ...SCRIPT_REFERENCE_REQUIRED
};

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Caredano', () => {
      describe('controller', () => {
        describe('script reference controller', () => {
          describe('create script reference', () => {
            let res: Response;
            let req: AppReq;
            let initModel: jest.Mock<void, [Sequelize]>;
            let bodyValidator: BodyValidator;
            let scriptReferenceFactory: jest.Mock<Either<AppError, ScriptReferenceItems>, [ScriptReferenceRequired]>;
            let scriptReference: ScriptReference;
            let sanitizeEntity: jest.Mock<any, [ScriptReference]>;
            let scriptReferenceService: ScriptReferenceService;
            let createScriptReferenceExecutor: jest.Mock<Promise<Either<AppError, ScriptReference>>, [ScriptReferenceItems]>;

            beforeAll(async () => {
              req = {
                body: BODY
              } as AppReq;

              res = {
                status: jest.fn().mockReturnThis()
              } as unknown as Response;
              initModel = jest.fn().mockImplementation(_ => null);
              sequelizeInitUnbound({
                scriptModel: initScriptModel,
                scriptReferenceModel: initScriptReferenceModel,
                userModel: initModel
              })(new Sequelize(SEQUELIZE_CONFIG));

              scriptReference = ScriptReference.build(SCRIPT_REFERENCE_ITEMS);

              bodyValidator = jest.fn().mockImplementation((body: ScriptReferenceBody) => Either.right(body));
              sanitizeEntity = jest.fn().mockImplementation((script: ScriptReference) => script.get({ plain: true }));
              createScriptReferenceExecutor = jest.fn().mockResolvedValue(Either.right(scriptReference));
              scriptReferenceFactory = jest.fn().mockReturnValue(Either.right(SCRIPT_REFERENCE_ITEMS));

              scriptReferenceService = {
                createScriptReference: jest.fn().mockImplementation(() => createScriptReferenceExecutor),
                getScriptReferences: null,
                getScriptReferenceById: null
              };

              await createScriptReferenceUnbound
                (bodyValidator, scriptReferenceFactory, sanitizeEntity)
                (scriptReferenceService)
                (null, req, res);
            });

            it('Should validate body as first', () => {
              expect(bodyValidator)
                .toHaveBeenCalledTimes(1);
              expect(bodyValidator)
                .toHaveBeenCalledWith(req.body);
            });

            it('Should calculate default script items, after body has been validated', async () => {
              expect(scriptReferenceFactory)
                .toHaveBeenCalledTimes(1);
              expect(scriptReferenceFactory)
                .toHaveBeenCalledWith(req.body);

              expect(scriptReferenceFactory)
                .toHaveBeenCalledAfter(bodyValidator);
            });

            it('Should create new script reference in DB (call service), after validation has been passed', async () => {
              expect(scriptReferenceService.createScriptReference)
                .toHaveBeenCalledTimes(1);
              expect(scriptReferenceService.createScriptReference)
                .toHaveBeenCalledWith();

              expect(createScriptReferenceExecutor)
                .toHaveBeenCalledTimes(1);
              expect(createScriptReferenceExecutor)
                .toHaveBeenCalledWith(SCRIPT_REFERENCE_ITEMS);

              expect(createScriptReferenceExecutor)
                .toHaveBeenCalledAfter(scriptReferenceFactory);
            });

            it('Should sanitize response and set response status to code 201', async () => {
              expect(res.status)
                .toHaveBeenCalledTimes(1);
              expect(res.status)
                .toHaveBeenCalledWith(201);

              expect(sanitizeEntity)
                .toHaveBeenCalledTimes(1);
              expect(sanitizeEntity)
                .toHaveBeenCalledWith(scriptReference);
            });
          });
        });
      });
    });
  });
});
