import getScriptReferencesUnbound from './getScriptReferences.unbound';
import { AppError } from 'common/error';
import sequelizeInitUnbound from 'model/sequelize/modelFactory/sequelizeInit/sequelizeInit.unbound';
import initScriptReferenceModel, { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { FindOptions, Options, Sequelize, WhereOptions } from 'sequelize';
import { Either } from 'tsmonad';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { SequelizeIncludes } from 'service/sequelize/types';
import { ScriptReferenceItems } from 'model/sequelize/model/scriptReference/scriptReference.types';
import { ScriptItems, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';
import { TransactionContext } from 'model/sequelize/modelFactory/modelFactory.types';
import { NotFound } from 'common/httpErrors';

const SEQUELIZE_CONFIG: Options = {
  dialect: EDatabaseDialect.sqlite,
};

const SCRIPT_REFERENCE_ITEMS: ScriptReferenceItems = {
  id: 'f923b2c9-ffcf-4a0a-bdc9-a4a4ae2a687e',
  scriptId: 'f0962fc9-882d-416d-bc08-fed1d5aa3a36',
  address: 'addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla',
  txId: '82e75104c2ffcab389fae6a9c87ebbe99e83cd7826d02534e77783b12c62e467',
  txIndex: 0
};

const SCRIPT_ITEMS: ScriptItems = {
  id: 'f0962fc9-882d-416d-bc08-fed1d5aa3a36',
  type: PlutusVersion.PlutusV2,
  script: '49480100002221200101',
  category: ScritpCategory.Gift,
  title: 'PPP',
  description: 'Example of gift script from PPP'
};

const INCLUDES: SequelizeIncludes = {
  include: [
    {
      model: Script,
      as: 'script',
    }
  ]
};

const WHERE: WhereOptions = { address: 'addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla' };

const CONTEXT: TransactionContext = { transaction: null };


describe('Service', () => {
  describe('Sequelize', () => {
    describe('ScriptReference Service', () => {
      describe(`Get script references`, () => {
        let findAll: jest.Mock<Promise<Either<AppError, ScriptReference[]>>, [FindOptions]>;
        let initModel: jest.Mock<void, [Sequelize]>;
        let scriptReference: ScriptReference;
        let result: Either<AppError, ScriptReference[]>;

        beforeAll(async () => {
          initModel = jest.fn().mockImplementation(_ => { });
          sequelizeInitUnbound({
            scriptModel: initScriptModel,
            scriptReferenceModel: initScriptReferenceModel,
            userModel: initModel
          })(new Sequelize(SEQUELIZE_CONFIG));

          scriptReference = ScriptReference.build({ ...SCRIPT_REFERENCE_ITEMS, script: SCRIPT_ITEMS }, { ...INCLUDES });
          findAll = jest.fn().mockResolvedValue(Either.right<AppError, ScriptReference[]>([scriptReference]));
        });

        describe('Happy path', () => {
          beforeAll(async () => {
            findAll = jest.fn().mockResolvedValue(Either.right<AppError, ScriptReference[]>([scriptReference]));
            result = await getScriptReferencesUnbound
              ({ create: null, findAll, findOne: null, findAndCountAll: null, findByPk: null, update: null, updateByPk: null, bulkCreate: null, destroy: null })
              (INCLUDES)
              (CONTEXT)
              (WHERE);
          });

          it(`Should find script references in storage and return Either with exact ScriptReference array in right side`, () => {
            result.do({
              right: (scriptReferences: ScriptReference[]): void => {
                const scriptReferenceItem = scriptReferences[0].get({ plain: true });

                expect(scriptReferences).toBeArrayOfSize(1);
                expect(scriptReferences[0]).toBeInstanceOf(ScriptReference);
                expect(scriptReferenceItem).toStrictEqual({
                  ...SCRIPT_REFERENCE_ITEMS,
                  createdAt: scriptReferenceItem.createdAt,
                  updatedAt: scriptReferenceItem.updatedAt,
                  script: {
                    ...SCRIPT_ITEMS,
                    createdAt: scriptReferenceItem.script.createdAt,
                    updatedAt: scriptReferenceItem.script.updatedAt
                  }
                });
              },
              left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
            });
          });
        });

        describe('Error path', () => {
          const appError: NotFound = new NotFound('not exists');
          beforeAll(async () => {
            findAll = jest.fn().mockResolvedValue(Either.left<AppError, ScriptReference>(appError));
            result = await getScriptReferencesUnbound
              ({ create: null, findAll, findOne: null, findAndCountAll: null, findByPk: null, update: null, updateByPk: null, bulkCreate: null, destroy: null })
              (INCLUDES)
              (CONTEXT)
              (WHERE);
          });

          it(`Should not find scripts in storage and return Either with exact error in left side`, () => {
            result.do({
              right: (): void => fail(`Right side has not been expected`),
              left: (error: AppError) => {
                expect(error).toBeInstanceOf(NotFound);
                expect(error.message).toEqual(appError.message);
              }
            });
          });
        });
      });
    });
  });
});
