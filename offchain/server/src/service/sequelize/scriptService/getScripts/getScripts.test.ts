import getScriptsUnbound from './getScripts.unbound';
import { AppError } from 'common/error';
import sequelizeInitUnbound from 'model/sequelize/modelFactory/sequelizeInit/sequelizeInit.unbound';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import initScriptReferenceModel from 'model/sequelize/model/scriptReference/scriptReference';
import { FindOptions, Options, Sequelize, WhereOptions } from 'sequelize';
import { Either } from 'tsmonad';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { NotFound } from 'common/httpErrors';
import { SequelizeIncludes } from 'service/sequelize/types';
import { ScriptItems, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';
import { ScriptReferenceItems } from 'model/sequelize/model/scriptReference/scriptReference.types';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import { TransactionContext } from 'model/sequelize/modelFactory/modelFactory.types';

const SEQUELIZE_CONFIG: Options = {
  dialect: EDatabaseDialect.sqlite
};

const SCRIPT_ITEMS: ScriptItems = {
  id: 'f923b2c9-ffcf-4a0a-bdc9-a4a4ae2a687e',
  type: PlutusVersion.PlutusV2,
  script: '49480100002221200101',
  category: ScritpCategory.Gift,
  title: 'PPP',
  description: 'Example of gift script from PPP'
};

const SCRIPT_REFERENCE_ITEMS: ScriptReferenceItems = {
  id: 'f923b2c9-ffcf-4a0a-bdc9-a4a4ae2a687e',
  scriptId: 'f0962fc9-882d-416d-bc08-fed1d5aa3a36',
  address: 'addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla',
  txId: '82e75104c2ffcab389fae6a9c87ebbe99e83cd7826d02534e77783b12c62e467',
  txIndex: 0,
  unspend: true
};

const INCLUDES: SequelizeIncludes = {
  include: [
    {
      model: ScriptReference,
      as: 'scriptReferences'
    }
  ]
};

const CONTEXT: TransactionContext = { transaction: null };
const WHERE: WhereOptions = { address: 'addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla' };

describe('Service', () => {
  describe('Sequelize', () => {
    describe('Script Service', () => {
      describe(`Get scripts`, () => {
        let findAll: jest.Mock<Promise<Either<AppError, Script[]>>, [FindOptions]>;
        let initModel: jest.Mock<void, [Sequelize]>;
        let script: Script;
        let result: Either<AppError, Script[]>;

        beforeAll(async () => {
          initModel = jest.fn().mockImplementation(_ => null);
          sequelizeInitUnbound({
            scriptModel: initScriptModel,
            scriptReferenceModel: initScriptReferenceModel,
            userModel: initModel
          })(new Sequelize(SEQUELIZE_CONFIG));

          script = Script.build({ ...SCRIPT_ITEMS, scriptReferences: [SCRIPT_REFERENCE_ITEMS] }, { ...INCLUDES });
        });

        describe('Happy path', () => {
          beforeAll(async () => {
            findAll = jest.fn().mockResolvedValue(Either.right<AppError, Script[]>([script]));
            result = await getScriptsUnbound
              ({ create: null, findAll, findOne: null, findAndCountAll: null, findByPk: null, update: null, updateByPk: null, bulkCreate: null, destroy: null })
              (INCLUDES)
              (CONTEXT)
              (WHERE);
          });

          it(`Should find scripts in storage and return Either with exact Script array in right side`, () => {
            result.do({
              right: (scripts: Script[]): void => {
                const scriptItems = scripts[0].get({ plain: true });

                expect(scripts).toBeArrayOfSize(1);
                expect(scripts[0]).toBeInstanceOf(Script);
                expect(scriptItems).toStrictEqual({
                  ...SCRIPT_ITEMS,
                  createdAt: scriptItems.createdAt,
                  updatedAt: scriptItems.updatedAt,
                  scriptReferences: [{
                    ...SCRIPT_REFERENCE_ITEMS,
                    createdAt: scriptItems.scriptReferences[0].createdAt,
                    updatedAt: scriptItems.scriptReferences[0].updatedAt
                  }]
                });
              },
              left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
            });
          });
        });

        describe('Error path', () => {
          const appError: NotFound = new NotFound('not exists');
          beforeAll(async () => {
            findAll = jest.fn().mockResolvedValue(Either.left<AppError, Script>(appError));
            result = await getScriptsUnbound
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
