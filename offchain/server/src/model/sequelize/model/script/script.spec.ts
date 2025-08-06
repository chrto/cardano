import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { Options, Sequelize } from 'sequelize';
import initScriptModel, { Script } from './scirpt';
import { ScriptItems, ScritpCategory } from './script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';

describe('sequelize model', () => {
  const spiedScriptInit = jest.spyOn(Script, 'init');
  let items: ScriptItems;
  let script: Script;

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

    items = {
      id: 'f923b2c9-ffcf-4a0a-bdc9-a4a4ae2a687e',
      type: PlutusVersion.PlutusV2,
      script: '49480100002221200101',
      category: ScritpCategory.Gift,
      title: 'PPP',
      description: 'Example of gift script from PPP'
    };

    script = Script.build(items);
  });

  describe('script', () => {
    it('Should init Script model', () => {
      expect(spiedScriptInit).toHaveBeenCalled();
    });

    it('after build it should have exact data', () => {
      expect(script).toHaveProperty('id');
      expect(script.id).toBe(items.id);

      expect(script).toHaveProperty('type');
      expect(script.type).toBe(PlutusVersion.PlutusV2);

      expect(script).toHaveProperty('script');
      expect(script.script).toBe(items.script);

      expect(script).toHaveProperty('category');
      expect(script.category).toBe(items.category);

      expect(script).toHaveProperty('title');
      expect(script.title).toBe(items.title);

      expect(script).toHaveProperty('description');
      expect(script.description).toBe(items.description);

      expect(script).toHaveProperty('createdAt');
      expect(script).toHaveProperty('updatedAt');
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
