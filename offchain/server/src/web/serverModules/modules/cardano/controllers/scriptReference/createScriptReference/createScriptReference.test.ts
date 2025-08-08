import createScriptReferenceUnbound from './createScriptReference.unbound';
import appLogger from 'logger/appLogger';
import initScriptReferenceModel, { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import initScriptModel from 'model/sequelize/model/script/scirpt';
import sequelizeInitUnbound from 'model/sequelize/modelFactory/sequelizeInit/sequelizeInit.unbound';

import scriptReferenceService from 'service/sequelize/scriptReferenceService/scriptReferenceService';
import { Response } from 'express';
import { Query as ExpressQuery } from 'express-serve-static-core';
import { Options, Sequelize } from 'sequelize';
import { AppRequest } from 'web/serverModules/types';
import { RequestImplicits } from '../../../paramHandlers/paramHandlers.types';
import bodyValidator from './validator/bodyValidator';
import scriptReferenceFactory from 'model/sequelize/model/scriptReference/factory/scriptReferenceFactory';
import sanitizeModel from 'model/sequelize/sanitizeModel/sanitizeModel';
import { ScriptReferenceBody } from './createScriptReference.types';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { ScriptReferenceItems, ScriptReferenceRequired } from 'model/sequelize/model/scriptReference/scriptReference.types';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { InvalidInput } from 'common/httpErrors';

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
            let scriptReference: ScriptReference;
            let result: Either<AppError, ScriptReference>;

            beforeAll(async () => {
              appLogger.error = (_) => appLogger; // disable logger

              initModel = jest.fn().mockImplementation(_ => null);
              sequelizeInitUnbound({
                scriptModel: initScriptModel,
                scriptReferenceModel: initScriptReferenceModel,
                userModel: initModel
              })(new Sequelize(SEQUELIZE_CONFIG));

              scriptReference = ScriptReference.build(SCRIPT_REFERENCE_ITEMS);
              res = {
                status: jest.fn().mockReturnThis()
              } as unknown as Response;
            });

            describe('Happy paty', () => {
              beforeAll(async () => {
                req = {
                  body: BODY
                } as AppReq;

                ScriptReference.create = jest.fn().mockResolvedValue(scriptReference);
                ScriptReference.findOne = jest.fn().mockResolvedValue(null);

                result = await createScriptReferenceUnbound
                  (bodyValidator, scriptReferenceFactory, sanitizeModel)
                  (scriptReferenceService())
                  (null, req, res);
              });

              it('Should create new script reference in DB', () => {
                result.do({
                  right: (scriptReference: ScriptReference) => {
                    expect(scriptReference).toBeObject;
                    expect(scriptReference).toStrictEqual({
                      ...SCRIPT_REFERENCE_ITEMS,
                      createdAt: scriptReference.createdAt,
                      updatedAt: scriptReference.updatedAt,
                    });

                  },
                  left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
                });
              });
            });

            describe('Error paty', () => {
              describe('Body validation', () => {
                beforeAll(async () => {
                  req = {
                    body: { ...BODY, address: '' }
                  } as unknown as AppReq;

                  result = await createScriptReferenceUnbound
                    (bodyValidator, scriptReferenceFactory, sanitizeModel)
                    (scriptReferenceService())
                    (null, req, res);
                });

                it('Should response with exact error, if body validation has not been passed', () => {
                  const ERROR_MESSAGE = 'Validation failed: ["Missing mandatory property address"]';

                  result.do({
                    right: (): void => fail(`Right side has not been expected`),
                    left: (error: AppError) => {
                      expect(error).toBeInstanceOf(AppError);
                      expect(error).toBeInstanceOf(InvalidInput);
                      expect(error.message).toEqual(ERROR_MESSAGE);
                    }
                  });
                });
              });
            });
          });
        });
      });
    });
  });
});
