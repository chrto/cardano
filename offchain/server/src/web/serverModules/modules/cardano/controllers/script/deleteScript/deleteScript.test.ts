import deleteScriptUnbound from './deleteScript.unbound';
import appLogger from 'logger/appLogger';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { Sequelize } from 'sequelize';
import { DEFAULT_DB_DIALECT } from 'src/defaults';
import { Context } from '../../../context/context.types';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { DeletedScript } from './deleteScript.types';
import { ScriptItems, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';
import scriptService from 'service/sequelize/scriptService/scriptService';

const ITEMS: ScriptItems = {
  id: 'f923b2c9-ffcf-4a0a-bdc9-a4a4ae2a687e',
  type: PlutusVersion.PlutusV2,
  script: '49480100002221200101',
  category: ScritpCategory.Gift,
  title: 'PPP',
  description: 'Example of gift script from PPP'
};

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Cardano', () => {
      describe('controller', () => {
        describe('script controller', () => {
          describe('delete script by id', () => {
            let sequelize: Sequelize;
            let script: Script;
            let context: Context;
            let result: Either<AppError, DeletedScript>;

            beforeAll(async () => {
              appLogger.error = (_) => appLogger; // disable logger

              sequelize = new Sequelize(null, null, null, { dialect: DEFAULT_DB_DIALECT });
              initScriptModel(sequelize);
              script = Script.build(ITEMS);
            });

            describe('Happy path', () => {
              beforeAll(async () => {
                jest.clearAllMocks();
                Script.destroy = jest.fn().mockResolvedValue(1);
                context = { implicits: { script } };

                result = await deleteScriptUnbound
                  ()
                  (scriptService())
                  (context, null, null);
              });

              it('Should delete script from DB.', () => {
                expect(Script.destroy)
                  .toHaveBeenCalledTimes(1);
                expect(Script.destroy)
                  .toHaveBeenCalledWith({ where: { id: script.id }, ...{} });
              });

              it(`Should return Either with exact object in right side`, () => {
                result.do({
                  right: (value: DeletedScript): void => {
                    expect(value)
                      .toEqual({ removedScriptId: ITEMS.id });
                  },
                  left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
                });
              });
            });

            describe('Errort path', () => {
              describe('Service', () => {
                const serviceError: AppError = new AppError('service.error', 'Internal server error');

                beforeAll(async () => {
                  jest.clearAllMocks();
                  Script.destroy = jest.fn().mockRejectedValue(serviceError);
                  context = { implicits: { script } };

                  result = await deleteScriptUnbound
                    ()
                    (scriptService())
                    (context, null, null);
                });

                it(`Should return Either with exact error in left side`, () => {
                  result.do({
                    right: (_value: DeletedScript): void => fail(`Right side has not been expected`),
                    left: (error: AppError) => {
                      expect(error)
                        .toBeInstanceOf(AppError);
                      expect(error.message)
                        .toEqual('Internal Server Error');
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
