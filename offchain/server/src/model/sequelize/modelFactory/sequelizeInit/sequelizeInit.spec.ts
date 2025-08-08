import { Options, Sequelize } from 'sequelize';
import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { InitModels } from './sequelizeInit.types';
import sequelizeInitUnbound from './sequelizeInit.unbound';
import initUserModel from '../../model/user/user';
import initScriptModel, { Script } from '../../model/script/scirpt';
import initScriptReferenceModel, { ScriptReference } from '../../model/scriptReference/scriptReference';

describe('sequelize model', () => {
  describe('model factory', () => {
    describe('init model', () => {
      let initModel: InitModels;
      let sequelize: Sequelize;
      let result: Sequelize;

      beforeAll(() => {
        const SEQUELIZE_CONFIG: Options = {
          dialect: EDatabaseDialect.sqlite,
          storage: null,
          logging: false,
          define: {
            timestamps: true
          }
        };
        sequelize = new Sequelize(SEQUELIZE_CONFIG);

        initModel = {
          userModel: jest.fn().mockImplementation(initUserModel),
          scriptModel: jest.fn().mockImplementation(initScriptModel),
          scriptReferenceModel: jest.fn().mockImplementation(initScriptReferenceModel)
        };

        result = sequelizeInitUnbound(initModel)(sequelize);
      });

      it('Should init User sequelize model', () => {
        expect(initModel.userModel).toHaveBeenCalledTimes(1);
      });

      it('Should init Script sequelize model', () => {
        expect(initModel.scriptModel).toHaveBeenCalledTimes(1);
      });

      it('Should init ScriptReference sequelize model', () => {
        expect(initModel.scriptReferenceModel).toHaveBeenCalledTimes(1);
      });

      it('Should return sequelize instance', () => {
        expect(result).toBeObject;
        expect(result).toStrictEqual(sequelize);
      });

      it('ScriptReference should have valid \'script\' associations item', () => {
        expect(ScriptReference.associations).toHaveProperty('script');

        expect(ScriptReference.associations.script).toHaveProperty('associationType');
        expect(ScriptReference.associations.script.associationType).toEqual('BelongsTo');

        expect(ScriptReference.associations.script).toHaveProperty('foreignKey');
        expect(ScriptReference.associations.script.foreignKey).toEqual('scriptId');

        expect(ScriptReference.associations.script).toHaveProperty('as');
        expect(ScriptReference.associations.script.as).toEqual('script');
      });

      it('ScriptReference should have valid \'script\' associations item', () => {
        expect(Script.associations).toHaveProperty('scriptReferences');

        expect(Script.associations.scriptReferences).toHaveProperty('associationType');
        expect(Script.associations.scriptReferences.associationType).toEqual('HasMany');

        expect(Script.associations.scriptReferences).toHaveProperty('as');
        expect(Script.associations.scriptReferences.as).toEqual('scriptReferences');
      });
    });
  });
});
