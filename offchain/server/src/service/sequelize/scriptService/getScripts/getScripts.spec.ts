import getScriptsUnbound from './getScripts.unbound';
import { AppError } from 'common/error';
import sequelizeInitUnbound from 'model/sequelize/modelFactory/sequelizeInit/sequelizeInit.unbound';
import initScriptReferenceModel from 'model/sequelize/model/scriptReference/scriptReference';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import { FindOptions, Options, Sequelize, WhereOptions } from 'sequelize';
import { Either } from 'tsmonad';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
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
    describe('Scripts Service', () => {
      describe(`Get scripts`, () => {
        let findAll: jest.Mock<Promise<Either<AppError, Script[]>>, [FindOptions]>;
        let initModel: jest.Mock<void, [Sequelize]>;
        let script: Script;

        beforeAll(async () => {
          initModel = jest.fn().mockImplementation(_ => null);
          sequelizeInitUnbound({
            scriptModel: initScriptModel,
            scriptReferenceModel: initScriptReferenceModel,
            userModel: initModel
          })(new Sequelize(SEQUELIZE_CONFIG));

          script = Script.build({ ...SCRIPT_ITEMS, scriptReferences: [SCRIPT_REFERENCE_ITEMS] }, { ...INCLUDES });
          findAll = jest.fn().mockResolvedValue(Either.right<AppError, Script[]>([script]));

          script = Script.build({ ...SCRIPT_ITEMS, scriptReferences: [SCRIPT_REFERENCE_ITEMS] }, { ...INCLUDES });

          await getScriptsUnbound
            ({ create: null, findAll, findOne: null, findAndCountAll: null, findByPk: null, update: null, updateByPk: null, bulkCreate: null, destroy: null })
            (INCLUDES)
            (CONTEXT)
            (WHERE);
        });

        it(`Should call findAll storage with exact parameters`, () => {
          expect(findAll)
            .toHaveBeenCalledTimes(1);
          expect(findAll)
            .toHaveBeenCalledWith(
              { where: WHERE, order: undefined, ...INCLUDES, ...CONTEXT }
            );
        });
      });
    });
  });
});
