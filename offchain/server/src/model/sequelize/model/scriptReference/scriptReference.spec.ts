import { Options, Sequelize } from 'sequelize';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import initScriptReferenceModel, { ScriptReference } from './scriptReference';
import initScriptModel, { Script } from '../script/scirpt';
import { ScriptReferenceItems } from './scriptReference.types';
import { PlutusVersion } from 'model/cardano/cardano.types';
import { ScriptItems, ScritpCategory } from '../script/script.types';

describe('sequelize model', () => {
  const spiedScriptReferenceInit = jest.spyOn(ScriptReference, 'init');

  let itemsScript: ScriptItems;
  let itemsScriptReference: ScriptReferenceItems;
  let script: Script;
  let scriptReference: ScriptReference;

  beforeAll(() => {
    const SEQUELIZE_CONFIG: Options = {
      dialect: EDatabaseDialect.sqlite,
      storage: null,
      logging: false,
      define: {
        timestamps: true
      }
    };

    initScriptModel(new Sequelize(SEQUELIZE_CONFIG));
    initScriptReferenceModel(new Sequelize(SEQUELIZE_CONFIG));

    itemsScript = {
      id: 'f923b2c9-ffcf-4a0a-bdc9-a4a4ae2a687e',
      type: PlutusVersion.PlutusV2,
      script: '49480100002221200101',
      category: ScritpCategory.Gift,
      title: 'PPP',
      description: 'Example of gift script from PPP'
    };

    itemsScriptReference = {
      id: 'f923b2c9-ffcf-4a0a-bdc9-a4a4ae2a687e',
      scriptId: 'f0962fc9-882d-416d-bc08-fed1d5aa3a36',
      address: 'addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla',
      txId: '82e75104c2ffcab389fae6a9c87ebbe99e83cd7826d02534e77783b12c62e467',
      txIndex: 0
    };

    script = Script.build(itemsScriptReference);
    scriptReference = ScriptReference.build(itemsScriptReference);
  });

  describe('script reference', () => {
    it('Should init ScriptReference model', () => {
      expect(spiedScriptReferenceInit).toHaveBeenCalled();
    });

    it('after build it should have exact data', () => {
      expect(scriptReference).toHaveProperty('id');
      expect(scriptReference.id).toBe(itemsScriptReference.id);

      expect(scriptReference).toHaveProperty('scriptId');
      expect(scriptReference.scriptId).toBe(itemsScriptReference.scriptId);

      expect(scriptReference).toHaveProperty('address');
      expect(scriptReference.address).toBe(itemsScriptReference.address);

      expect(scriptReference).toHaveProperty('txId');
      expect(scriptReference.txId).toBe(itemsScriptReference.txId);

      expect(scriptReference).toHaveProperty('txIndex');
      expect(scriptReference.txIndex).toBe(itemsScriptReference.txIndex);

      expect(scriptReference).toHaveProperty('createdAt');
      expect(scriptReference).toHaveProperty('updatedAt');
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
