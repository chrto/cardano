import scriptParamHandlerUnbound from './scriptParamHandler.unbound';
import { AppError } from 'common/error';
import { NextFunction, Response } from 'express';
import { User } from 'model/sequelize/model/user/user';
import { Script } from 'model/sequelize/model/script/scirpt';
import { ScriptService } from 'service/sequelize/scriptService/scriptService.types';
import { Either } from 'tsmonad';
import { AppRequest } from 'web/serverModules/types';
import { RequestImplicits } from '../paramHandlers.types';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { InvalidInput } from 'common/httpErrors';

type LeftSideExecutor = jest.Mock<void, [AppError]>;
type RightSideExecutor = jest.Mock<void, [Script]>;

const SCRIPT_ID: string = '0a0b44eb-97ed-4f41-bbf1-fe01e93efb34';
const SCRIPT: Script = {
  id: SCRIPT_ID
} as Script;

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Portal', () => {
      describe('Request parameter handlers', () => {
        describe('Script', () => {
          let leftSideExecutor: LeftSideExecutor;
          let handleError: jest.Mock<LeftSideExecutor, [AppRequest<User, RequestImplicits>, Response, NextFunction, string]>;
          let rightSideExecutor: RightSideExecutor;
          let addEntityInToRequestImplicits: jest.Mock<RightSideExecutor, [AppRequest<User, RequestImplicits>, Response, NextFunction, string]>;
          let isUuid: jest.Mock<boolean, [any]>;

          let serviceExecutor: jest.Mock<Promise<Either<AppError, Script>>, [string]>;
          let scriptService: ScriptService = {} as ScriptService;

          beforeAll(() => {
            leftSideExecutor = jest.fn().mockReturnValue(null);
            handleError = jest.fn().mockReturnValue(leftSideExecutor);
            rightSideExecutor = jest.fn().mockReturnValue(null);
            addEntityInToRequestImplicits = jest.fn().mockReturnValue(rightSideExecutor);
            isUuid = jest.fn().mockReturnValue(true);

            serviceExecutor = jest.fn().mockResolvedValue(Either.right(SCRIPT));
            scriptService.getScriptById = jest.fn().mockReturnValue(serviceExecutor);
          });

          describe('Happy path', () => {
            beforeAll(async () => {
              jest.clearAllMocks();
              await scriptParamHandlerUnbound
                (addEntityInToRequestImplicits, handleError, isUuid)
                ({ scriptService } as PluginSdkService)
                (null, null, null, SCRIPT_ID);
            });

            it(`Should check, if script id is valid uuid format`, () => {
              expect(isUuid)
                .toHaveBeenCalledTimes(1);
              expect(isUuid)
                .toHaveBeenCalledWith(SCRIPT_ID);
            });

            it(`Should call scriptService, if uuid format check has been passed`, () => {
              expect(scriptService.getScriptById)
                .toHaveBeenCalledTimes(1);
              expect(scriptService.getScriptById)
                .toHaveBeenCalledWith();
              expect(serviceExecutor)
                .toHaveBeenCalledTimes(1);
              expect(serviceExecutor)
                .toHaveBeenCalledWith(SCRIPT_ID);

              expect(serviceExecutor)
                .toHaveBeenCalledAfter(isUuid);
            });

            it(`Should handle Either right side branch`, () => {
              expect(addEntityInToRequestImplicits)
                .toHaveBeenCalledTimes(1);
              expect(addEntityInToRequestImplicits)
                .toHaveBeenCalledWith(null, null, null, 'script');
              expect(rightSideExecutor)
                .toHaveBeenCalledTimes(1);
              expect(rightSideExecutor)
                .toHaveBeenCalledWith(SCRIPT);

              expect(rightSideExecutor)
                .toHaveBeenCalledAfter(serviceExecutor);

              expect(leftSideExecutor)
                .toHaveBeenCalledTimes(0);
            });
          });

          describe('Error path', () => {
            describe('script id check failed', () => {
              beforeAll(async () => {
                jest.clearAllMocks();
                isUuid = jest.fn().mockReturnValue(false);
                await scriptParamHandlerUnbound
                  (addEntityInToRequestImplicits, handleError, isUuid)
                  ({ scriptService } as PluginSdkService)
                  (null, null, null, SCRIPT_ID);
              });

              it('Should not continue in chain execution, if uuid check has not been passed', () => {
                expect(isUuid)
                  .toHaveBeenCalledTimes(1);
                expect(serviceExecutor)
                  .toHaveBeenCalledTimes(0);
              });

              it('Should handle Either left side branch, with exact error', () => {
                expect(rightSideExecutor)
                  .toHaveBeenCalledTimes(0);
                expect(leftSideExecutor)
                  .toHaveBeenCalledTimes(1);
                expect(leftSideExecutor)
                  .toHaveBeenCalledWith(new InvalidInput(`scriptId ${SCRIPT_ID} is not valid uuid`));
              });
            });

            describe('script service failed', () => {
              const error: AppError = new AppError('server.error', 'server error');
              beforeAll(async () => {
                jest.clearAllMocks();
                isUuid = jest.fn().mockReturnValue(true);
                serviceExecutor = jest.fn().mockResolvedValue(Either.left(error));
                scriptService.getScriptById = jest.fn().mockReturnValue(serviceExecutor);

                await scriptParamHandlerUnbound
                  (addEntityInToRequestImplicits, handleError, isUuid)
                  ({ scriptService } as PluginSdkService)
                  (null, null, null, SCRIPT_ID);
              });

              it('Should handle Either left side branch, with exact error', () => {
                expect(rightSideExecutor)
                  .toHaveBeenCalledTimes(0);
                expect(leftSideExecutor)
                  .toHaveBeenCalledTimes(1);
                expect(leftSideExecutor)
                  .toHaveBeenCalledWith(error);
              });
            });
          });
        });
      });
    });
  });
});
