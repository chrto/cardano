import { Options, Sequelize } from 'sequelize';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import sequelizeInitUnbound from 'model/sequelize/modelFactory/sequelizeInit/sequelizeInit.unbound';
import initScriptReferenceModel, { ScriptReference } from './scriptReference';
import initScriptModel, { Script } from '../script/scirpt';
import { ScriptReferenceItems } from './scriptReference.types';
import { ScriptItems, ScritpCategory } from '../script/script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';
import { SequelizeIncludes } from 'service/sequelize/types';

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

describe('sequelize model', () => {
  const spiedScriptReferenceInit = jest.spyOn(ScriptReference, 'init');

  let initModel: jest.Mock<void, [Sequelize]>;
  let scriptReference: ScriptReference;

  beforeAll(() => {
    initModel = jest.fn().mockImplementation(_ => { });
    sequelizeInitUnbound({
      scriptModel: initScriptModel,
      scriptReferenceModel: initScriptReferenceModel,
      userModel: initModel
    })(new Sequelize(SEQUELIZE_CONFIG));

    scriptReference = ScriptReference.build({ ...SCRIPT_REFERENCE_ITEMS, script: SCRIPT_ITEMS }, { ...INCLUDES });
    scriptReference; //?
  });

  describe('script reference', () => {
    it('Should init ScriptReference model', () => {
      expect(spiedScriptReferenceInit).toHaveBeenCalled();
    });

    it('after build it should have exact data', () => {
      expect(scriptReference).toHaveProperty('id');
      expect(scriptReference.id).toBe(SCRIPT_REFERENCE_ITEMS.id);

      expect(scriptReference).toHaveProperty('scriptId');
      expect(scriptReference.scriptId).toBe(SCRIPT_REFERENCE_ITEMS.scriptId);

      expect(scriptReference).toHaveProperty('address');
      expect(scriptReference.address).toBe(SCRIPT_REFERENCE_ITEMS.address);

      expect(scriptReference).toHaveProperty('txId');
      expect(scriptReference.txId).toBe(SCRIPT_REFERENCE_ITEMS.txId);

      expect(scriptReference).toHaveProperty('txIndex');
      expect(scriptReference.txIndex).toBe(SCRIPT_REFERENCE_ITEMS.txIndex);

      expect(scriptReference).toHaveProperty('createdAt');
      expect(scriptReference).toHaveProperty('updatedAt');

      expect(scriptReference).toHaveProperty('script');

      expect(scriptReference['script']).toHaveProperty('id');
      expect(scriptReference['script'].id).toBe(SCRIPT_ITEMS.id);

      expect(scriptReference['script']).toHaveProperty('type');
      expect(scriptReference['script'].type).toBe(SCRIPT_ITEMS.type);

      expect(scriptReference['script']).toHaveProperty('script');
      expect(scriptReference['script'].script).toBe(SCRIPT_ITEMS.script);

    });

    it('Should update script reference items', () => {
      scriptReference.setAttributes({
        txIndex: 5
      });

      expect(scriptReference.txIndex).toBe(5);
    });

    it('should have exact table name', () => {
      expect(ScriptReference.getTableName()).toBe('script_references');
    });
  });
});
