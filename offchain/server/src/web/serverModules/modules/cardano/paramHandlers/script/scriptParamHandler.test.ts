import scriptParamHandlerUnbound from './scriptParamHandler.unbound';
import { AppError } from 'common/error';
import { NextFunction, Response } from 'express';
import { Script } from 'model/sequelize/model/script/scirpt';
import { User } from 'model/sequelize/model/user/user';
import { ScriptService } from 'service/sequelize/scriptService/scriptService.types';
import { Either } from 'tsmonad';
import { AppRequest } from 'web/serverModules/types';
import { RequestImplicits } from '../paramHandlers.types';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import addEntityInToRequestImplicits from 'web/serverModules/common/paramHandlers/addEntityInToRequestImplicits/addEntityInToRequestImplicits';
import handleError from 'web/serverModules/common/paramHandlers/handleError/handleError';
import { isUuid } from 'utils/validation';
import { Fcn } from 'common/types';
import { InvalidInput } from 'common/httpErrors';

const SCRIPT_ID: string = '0a0b44eb-97ed-4f41-bbf1-fe01e93efb34';
const SCRIPT: Script = {
  id: SCRIPT_ID
} as Script;

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Cardano', () => {
      describe('Request parameter handlers', () => {
        describe('Script', () => {
          let nextFunction: jest.Mock<void, [any]>;
          let serviceExecutro: jest.Mock<Promise<Either<AppError, Script>>, [string]>;
          let scriptService: ScriptService = {} as ScriptService;
          let scriptParamHandler: Fcn<[AppRequest<User, RequestImplicits>, Response, NextFunction, string], Promise<void>>;

          beforeAll(() => {
            nextFunction = jest.fn().mockReturnValue(null);
            serviceExecutro = jest.fn().mockResolvedValue(Either.right(SCRIPT));
            scriptService.getScriptById = jest.fn().mockReturnValue(serviceExecutro);

            scriptParamHandler = scriptParamHandlerUnbound
              (addEntityInToRequestImplicits, handleError, isUuid)
              ({ scriptService } as PluginSdkService);
          });

          describe('Happy path', () => {
            let req: AppRequest<User, RequestImplicits> = {} as AppRequest<User, RequestImplicits>;
            beforeAll(async () => {
              jest.clearAllMocks();
              await scriptParamHandler(req, null, nextFunction, SCRIPT_ID);
            });

            it(`Should handle Either right side branch`, () => {
              expect(nextFunction).toHaveBeenCalledTimes(1);
              expect(nextFunction).toHaveBeenCalledWith();
            });

            it(`Should add script in to request implicits`, () => {
              expect(req).toHaveProperty('implicits');
              expect(req.implicits).toHaveProperty('script');
              expect(req.implicits.script).toStrictEqual(SCRIPT);
            });
          });

          describe('Error path', () => {
            describe('script id check failed', () => {
              const scriptId: string = 'wrong id format';
              beforeAll(async () => {
                jest.clearAllMocks();
                await scriptParamHandler(null, null, nextFunction, scriptId);
              });

              it(`Should handle Either left side branch and pass exact error in to next middleware`, () => {
                expect(nextFunction).toHaveBeenCalledTimes(1);
                expect(nextFunction).toHaveBeenCalledWith(new InvalidInput(`scriptId ${scriptId} is not valid uuid`));
              });
            });

            describe('script service failed', () => {
              const error: AppError = new AppError('server error', 'internal server error');

              beforeAll(async () => {
                jest.clearAllMocks();
                serviceExecutro = jest.fn().mockResolvedValue(Either.left(error));
                scriptService.getScriptById = jest.fn().mockReturnValue(serviceExecutro);
                await scriptParamHandlerUnbound
                  (addEntityInToRequestImplicits, handleError, isUuid)
                  ({ scriptService } as PluginSdkService)
                  (null, null, nextFunction, SCRIPT_ID);
              });

              it(`Should handle Either left side branch and pass exact error in to next middleware`, () => {
                expect(nextFunction).toHaveBeenCalledTimes(1);
                expect(nextFunction).toHaveBeenCalledWith(error);
              });
            });
          });
        });
      });
    });
  });
});
