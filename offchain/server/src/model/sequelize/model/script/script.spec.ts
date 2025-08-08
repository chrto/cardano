import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { Options, Sequelize } from 'sequelize';
import sequelizeInitUnbound from 'model/sequelize/modelFactory/sequelizeInit/sequelizeInit.unbound';
import initScriptReferenceModel, { ScriptReference } from '../scriptReference/scriptReference';
import initScriptModel, { Script } from './scirpt';
import { ScriptItems, ScritpCategory } from './script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';
import { ScriptReferenceItems } from '../scriptReference/scriptReference.types';
import { SequelizeIncludes } from 'service/sequelize/types';

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
      model: ScriptReference,
      as: 'scriptReferences'
    }
  ]
};

describe('sequelize model', () => {
  const spiedScriptInit = jest.spyOn(Script, 'init');
  let initModel: jest.Mock<void, [Sequelize]>;
  let script: Script;

  beforeAll(() => {
    initModel = jest.fn().mockReturnValue(null);
    sequelizeInitUnbound({
      scriptModel: initScriptModel,
      scriptReferenceModel: initScriptReferenceModel,
      userModel: initModel
    })(new Sequelize(SEQUELIZE_CONFIG));

    script = Script.build({ ...SCRIPT_ITEMS, scriptReferences: [SCRIPT_REFERENCE_ITEMS] }, { ...INCLUDES });
  });

  describe('script', () => {
    it('Should init Script model', () => {
      expect(spiedScriptInit).toHaveBeenCalled();
    });

    it('after build it should have exact data', () => {
      expect(script).toHaveProperty('id');
      expect(script.id).toBe(SCRIPT_ITEMS.id);

      expect(script).toHaveProperty('type');
      expect(script.type).toBe(PlutusVersion.PlutusV2);

      expect(script).toHaveProperty('script');
      expect(script.script).toBe(SCRIPT_ITEMS.script);

      expect(script).toHaveProperty('category');
      expect(script.category).toBe(SCRIPT_ITEMS.category);

      expect(script).toHaveProperty('title');
      expect(script.title).toBe(SCRIPT_ITEMS.title);

      expect(script).toHaveProperty('description');
      expect(script.description).toBe(SCRIPT_ITEMS.description);

      expect(script).toHaveProperty('createdAt');
      expect(script).toHaveProperty('updatedAt');

      expect(script).toHaveProperty('scriptReferences');
      expect(script['scriptReferences']).toBeArrayOfSize(1);
      expect(script['scriptReferences'][0].id).toBe(SCRIPT_REFERENCE_ITEMS.id);
    });

    it('Should update script items', () => {
      script.setAttributes({
        category: ScritpCategory.FortyTwo,
        title: 'FortyTwo PPP',
        description: '...'
      });

      expect(script.category).toBe(ScritpCategory.FortyTwo);
      expect(script.title).toBe('FortyTwo PPP');
      expect(script.description).toBe('...');
    });

    it('should have exact table name', () => {
      expect(Script.getTableName()).toBe('scripts');
    });
  });
});
