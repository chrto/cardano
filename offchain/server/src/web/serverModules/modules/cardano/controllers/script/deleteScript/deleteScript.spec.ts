import deleteScriptUnbound from './deleteScript.unbound';
import { AppError } from 'common/error';
import { Script } from 'model/sequelize/model/script/scirpt';
import { Either } from 'tsmonad';
import { Context } from '../../../context/context.types';
import { DeletedScript } from './deleteScript.types';

type DeleteScriptExecutor = jest.Mock<Promise<Either<AppError, number>>, [Script]>;
const SCRIPT_MODEL: Script = { id: '3a2cffc9-1fc5-4a05-87d8-8411cd4f920c' } as Script;

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Cardano', () => {
      describe('controller', () => {
        describe('script controller', () => {
          describe('delete script by id', () => {
            let deleteScriptExecutor: DeleteScriptExecutor;
            let deleteScript: jest.Mock<DeleteScriptExecutor, []>;

            let context: Context;
            let result: Either<AppError, DeletedScript>;

            beforeAll(async () => {
              context = { implicits: { script: SCRIPT_MODEL } };
            });

            describe('Happy path', () => {
              beforeAll(async () => {
                jest.clearAllMocks();
                deleteScriptExecutor = jest.fn().mockResolvedValue(Either.right(1));
                deleteScript = jest.fn().mockReturnValue(deleteScriptExecutor);

                result = await deleteScriptUnbound
                  ()
                  ({ deleteScript, createScript: null, createScriptReference: null, getScripts: null, getScriptById: null })
                  (context, null, null);
              });

              it(`Should delete script from DB`, () => {
                expect(deleteScript)
                  .toHaveBeenCalledTimes(1);
                expect(deleteScript)
                  .toHaveBeenCalledWith();
                expect(deleteScriptExecutor)
                  .toHaveBeenCalledTimes(1);
                expect(deleteScriptExecutor)
                  .toHaveBeenCalledWith(SCRIPT_MODEL);
              });

              it(`Should return Either with exact object in right side`, () => {
                result.do({
                  right: (value: DeletedScript): void => {
                    expect(value)
                      .toEqual({ removedScriptId: SCRIPT_MODEL.id });
                  },
                  left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
                });
              });
            });

            describe('Error path', () => {
              describe('Service', () => {
                const ERROR_MESSAGE: string = 'Internal server error';
                const serviceError: AppError = new AppError('service.error', ERROR_MESSAGE);

                beforeAll(async () => {
                  jest.clearAllMocks();
                  deleteScriptExecutor = jest.fn().mockResolvedValue(Either.left(serviceError));
                  deleteScript = jest.fn().mockReturnValue(deleteScriptExecutor);

                  result = await deleteScriptUnbound
                    ()
                    ({ deleteScript, createScript: null, createScriptReference: null, getScripts: null, getScriptById: null })
                    (context, null, null);
                });

                it('Should return Either with exact error in left side', () => {
                  result.do({
                    right: (_value: DeletedScript): void => fail(`Right side has not been expected`),
                    left: (error: AppError) => {
                      expect(error)
                        .toBeInstanceOf(AppError);
                      expect(error.message)
                        .toEqual(ERROR_MESSAGE);
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
