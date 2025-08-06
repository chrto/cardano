import createScriptReferenceUnbound from './createScriptReference.unbound';
import initScriptReferenceModel, { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import initScriptModel from 'model/sequelize/model/script/scirpt';
import sequelizeInitUnbound from 'model/sequelize/modelFactory/sequelizeInit/sequelizeInit.unbound';
import { AppError } from 'common/error';
import { CreateOptions, Options, Sequelize } from 'sequelize';
import { Either } from 'tsmonad';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { ScriptReferenceItems } from 'model/sequelize/model/scriptReference/scriptReference.types';

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

describe('Service', () => {
  describe('Sequelize', () => {
    describe('ScriptReference Service', () => {
      describe(`Create new script reference`, () => {
        let createScriptReferenceModel: jest.Mock<Promise<Either<AppError, ScriptReference>>, [object, CreateOptions]>;
        let scriptReference: ScriptReference;
        let initModel: jest.Mock<void, [Sequelize]>;

        beforeAll(async () => {
          initModel = jest.fn().mockImplementation(_ => {});
          sequelizeInitUnbound({
            scriptModel: initScriptModel,
            scriptReferenceModel: initScriptReferenceModel,
            userModel: initModel
          })(new Sequelize(SEQUELIZE_CONFIG))

          scriptReference = ScriptReference.build(SCRIPT_REFERENCE_ITEMS);

          createScriptReferenceModel = jest.fn().mockResolvedValue(Either.right<AppError, ScriptReference>(scriptReference));

          await createScriptReferenceUnbound
            ({ create: createScriptReferenceModel, findAll: null, findOne: null, findAndCountAll: null, findByPk: null, update: null, updateByPk: null, bulkCreate: null, destroy: null })
            ()
            (SCRIPT_REFERENCE_ITEMS);
        });

        it(`Should call create storage with exact parameters`, () => {
          expect(createScriptReferenceModel)
            .toHaveBeenCalledTimes(1);
          expect(createScriptReferenceModel)
            .toHaveBeenCalledWith(SCRIPT_REFERENCE_ITEMS, undefined);
        });
      });
    });
  });
});
