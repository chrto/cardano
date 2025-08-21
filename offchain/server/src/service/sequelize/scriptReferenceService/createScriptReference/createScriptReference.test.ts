import createScriptReferenceUnbound from './createScriptReference.unbound';
import initScriptReferenceModel, { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import sequelizeInitUnbound from 'model/sequelize/modelFactory/sequelizeInit/sequelizeInit.unbound';
import initScriptModel from 'model/sequelize/model/script/scirpt';
import { AppError } from 'common/error';
import { CreateOptions, Options, Sequelize } from 'sequelize';
import { Either } from 'tsmonad';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { Conflict } from 'common/httpErrors';
import { ScriptReferenceItems } from 'model/sequelize/model/scriptReference/scriptReference.types';

const SEQUELIZE_CONFIG: Options = {
  dialect: EDatabaseDialect.sqlite
};

const SCRIPT_REFERENCE_ITEMS: ScriptReferenceItems = {
  id: 'f923b2c9-ffcf-4a0a-bdc9-a4a4ae2a687e',
  scriptId: 'f0962fc9-882d-416d-bc08-fed1d5aa3a36',
  address: 'addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla',
  txId: '82e75104c2ffcab389fae6a9c87ebbe99e83cd7826d02534e77783b12c62e467',
  txIndex: 0,
  unspend: true
};

describe('Service', () => {
  describe('Sequelize', () => {
    describe('ScriptReference Service', () => {
      describe(`Create new script reference`, () => {
        let createScriptReferenceModel: jest.Mock<Promise<Either<AppError, ScriptReference>>, [object, CreateOptions]>;
        let initModel: jest.Mock<void, [Sequelize]>;
        let result: Either<AppError, ScriptReference>;

        beforeAll(async () => {
          initModel = jest.fn().mockImplementation(_ => null);
          sequelizeInitUnbound({
            scriptModel: initScriptModel,
            scriptReferenceModel: initScriptReferenceModel,
            userModel: initModel
          })(new Sequelize(SEQUELIZE_CONFIG));
        });

        describe('Happy path', () => {
          beforeAll(async () => {
            createScriptReferenceModel = jest.fn().mockResolvedValue(Either.right<AppError, ScriptReference>(ScriptReference.build(SCRIPT_REFERENCE_ITEMS)));
            result = await createScriptReferenceUnbound
              ({ create: createScriptReferenceModel, findAll: null, findOne: null, findAndCountAll: null, findByPk: null, update: null, updateByPk: null, bulkCreate: null, destroy: null })
              ()
              (SCRIPT_REFERENCE_ITEMS);
          });

          it(`Should create new script reference in storage and return Either with ScriptReference model in right side`, () => {
            result.do({
              right: (scriptReference: ScriptReference): void => {
                const scriptReferenceItems = scriptReference.get();
                expect(scriptReference).toBeInstanceOf(ScriptReference);
                expect(scriptReferenceItems).toStrictEqual({
                  ...SCRIPT_REFERENCE_ITEMS,
                  createdAt: scriptReferenceItems.createdAt,
                  updatedAt: scriptReferenceItems.updatedAt
                });
              },
              left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
            });
          });
        });

        describe('Error path', () => {
          const appError: Conflict = new Conflict('script exists');
          beforeAll(async () => {
            createScriptReferenceModel = jest.fn().mockResolvedValue(Either.left<AppError, ScriptReference>(appError));
            result = await createScriptReferenceUnbound
              ({ create: createScriptReferenceModel, findAll: null, findOne: null, findAndCountAll: null, findByPk: null, update: null, updateByPk: null, bulkCreate: null, destroy: null })
              ()
              (SCRIPT_REFERENCE_ITEMS);
          });

          it(`Should not create new script reference in storage and return Either with exact error in left side`, () => {
            result.do({
              right: (): void => fail(`Right side has not been expected`),
              left: (error: AppError) => {
                expect(error).toBeInstanceOf(Conflict);
                expect(error.message).toEqual(appError.message);
              }
            });
          });
        });
      });
    });
  });
});
