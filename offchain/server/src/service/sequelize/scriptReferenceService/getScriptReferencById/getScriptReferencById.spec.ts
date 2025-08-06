import getScriptReferencByIdUnbound from './getScriptReferencById.unbound';
import initScriptReferenceModel, { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import initScriptModel, { Script } from 'model/sequelize/model/script/scirpt';
import sequelizeInitUnbound from 'model/sequelize/modelFactory/sequelizeInit/sequelizeInit.unbound';
import { AppError } from 'common/error';
import { FindOptions, Options, Sequelize } from 'sequelize';
import { Either } from 'tsmonad';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { SequelizeIncludes } from 'service/sequelize/types';
import { NotFound } from 'common/httpErrors';
import { ScriptReferenceItems } from 'model/sequelize/model/scriptReference/scriptReference.types';
import { ScriptItems, ScritpCategory } from 'model/sequelize/model/script/script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';
import { TransactionContext } from 'model/sequelize/modelFactory/modelFactory.types';

const SEQUELIZE_CONFIG: Options = {
  dialect: EDatabaseDialect.sqlite
};

const SCRIPT_REFERENCE_ITEMS: ScriptReferenceItems = {
  id: 'f923b2c9-ffcf-4a0a-bdc9-a4a4ae2a687e',
  scriptId: 'f0962fc9-882d-416d-bc08-fed1d5aa3a36',
  address: 'addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla',
  txId: '82e75104c2ffcab389fae6a9c87ebbe99e83cd7826d02534e77783b12c62e467',
  txIndex: 0
};

const SCRIPT_ITEMS: ScriptItems = {
  id: 'f923b2c9-ffcf-4a0a-bdc9-a4a4ae2a687e',
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
      as: 'script'
    }
  ]
};

const CONTEXT: TransactionContext = {
  transaction: null
};

describe('Service', () => {
  describe('Sequelize', () => {
    describe('ScriptReference Service', () => {
      describe(`Get script by id`, () => {
        let findByPk: jest.Mock<Promise<Either<AppError, ScriptReference>>, [string, FindOptions, AppError]>;
        let initModel: jest.Mock<void, [Sequelize]>;
        let scriptReference: ScriptReference;

        beforeAll(async () => {
          initModel = jest.fn().mockImplementation(_ => { });
          sequelizeInitUnbound({
            scriptModel: initScriptModel,
            scriptReferenceModel: initScriptReferenceModel,
            userModel: initModel
          })(new Sequelize(SEQUELIZE_CONFIG));

          scriptReference = ScriptReference.build({ ...SCRIPT_REFERENCE_ITEMS, script: SCRIPT_ITEMS }, { ...INCLUDES });


          findByPk = jest.fn().mockResolvedValue(Either.right<AppError, ScriptReference>(scriptReference));

          await getScriptReferencByIdUnbound
            ({ create: null, findAll: null, findOne: null, findAndCountAll: null, findByPk, update: null, updateByPk: null, bulkCreate: null, destroy: null })
            (INCLUDES)
            (CONTEXT)
            (SCRIPT_REFERENCE_ITEMS.id);
        });

        it(`Should call findByPk storage with exact parameters`, () => {
          expect(findByPk)
            .toHaveBeenCalledTimes(1);
          expect(findByPk)
            .toHaveBeenCalledWith(
              SCRIPT_REFERENCE_ITEMS.id,
              { ...INCLUDES, ...CONTEXT },
              new NotFound(`Cannot find script reference identified by id = '${SCRIPT_REFERENCE_ITEMS.id}'`)
            );
        });
      });
    });
  });
});
