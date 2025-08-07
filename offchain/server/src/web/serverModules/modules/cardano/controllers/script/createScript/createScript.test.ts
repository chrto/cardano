import createScriptUnbound from './createScript.unbound';
import { ScriptBody } from './createScript.types';
import doer from 'utils/monad/either/do/doer';
import appLogger from 'logger/appLogger';
import scriptService from 'service/sequelize/scriptService/scriptService';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { User } from 'model/sequelize/model/user/user';
import { Response } from 'express';
import { Sequelize, Transaction } from 'sequelize';
import { DEFAULT_DB_DIALECT } from 'src/defaults';
import { AppError } from 'common/error';
import { AppRequest } from 'web/serverModules/types';
import { RequestImplicits } from '../../../paramHandlers/paramHandlers.types';
import { InvalidInput } from 'common/httpErrors';
import bodyValidator from './validator/bodyValidator';
import scriptFactory from 'model/sequelize/model/script/factory/scriptFactory';
import sanitizeModel from 'model/sequelize/sanitizeModel/sanitizeModel';
import { ScriptItems, ScriptRequired, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';
import { SdkTransaction } from 'model/sequelize/modelFactory/modelFactory.types';
import { Either } from 'tsmonad';

type AppReq = AppRequest<User, RequestImplicits, unknown, ScriptBody>;
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

const TRANSACTION = {} as Transaction;

const SDK_TRANSACTION: SdkTransaction = {
  begin: () => Promise.resolve(TRANSACTION),
  commitOrRollback: (_tx: Transaction) => <T> (valOrErr: Either<AppError, T>) => Promise.resolve(valOrErr),
  rollback: (_tx: Transaction) => (err: AppError) => Promise.reject(err)
};

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Caredano', () => {
      describe('controller', () => {
        describe('script controller', () => {
          describe('create script', () => {
            let res: Partial<Response>;
            let req: AppReq;

            let sequelize: Sequelize;
            let script: Script;
            let createScript;
            beforeAll(() => {
              appLogger.error = (_) => appLogger; // disable logger
              sequelize = new Sequelize(null, null, null, { dialect: DEFAULT_DB_DIALECT });
              initScriptModel(sequelize);

              script = Script.build(SCRIPT_ITEMS);
              res = {
                status: jest.fn().mockReturnThis()
              };

              createScript = createScriptUnbound
                (bodyValidator, scriptFactory, sanitizeModel)
                (scriptService(SDK_TRANSACTION));
            });

            describe('Happy paty', () => {
              beforeAll(() => {
                req = {
                  body: BODY
                } as AppReq;

                Script.create = jest.fn().mockResolvedValue(script);
                Script.findOne = jest.fn().mockResolvedValue(null);
              });
              it('Should create new script in DB', () => {
                createScript
                  .apply(null, [null, req, res])
                  .then(doer({
                    right: (result: Script) => {
                      expect(result).toBeObject;
                      expect(result).toStrictEqual(script.get({}));

                    },
                    left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
                  }));
              });
            });

            describe('Error paty', () => {
              describe('Body validation', () => {
                beforeAll(() => {
                  req = {
                    body: { ...BODY, title: '' }
                  } as AppReq;
                });
                it('Should response with exact error, if body validation has not been passed', () => {
                  const ERROR_MESSAGE = 'Validation failed: ["Missing mandatory property title"]';
                  createScript
                    .apply(null, [null, req, res])
                    .then(doer({
                      right: (): void => fail(`Right side has not been expected`),
                      left: (error: AppError) => {
                        expect(error).toBeInstanceOf(InvalidInput);
                        expect(error.message).toEqual(ERROR_MESSAGE);
                      }
                    }));
                });
              });
            });
          });
        });
      });
    });
  });
});
