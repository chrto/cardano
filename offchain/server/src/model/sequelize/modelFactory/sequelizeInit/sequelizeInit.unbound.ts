import { Sequelize } from 'sequelize';
import { InitModels } from './sequelizeInit.types';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import { Script } from 'model/sequelize/model/script/scirpt';

export default (initModel: InitModels) =>
  (sequelize: Sequelize): Sequelize => {
    initModel.userModel(sequelize);
    initModel.scriptModel(sequelize);
    initModel.scriptReferenceModel(sequelize);

    ScriptReference.belongsTo(Script, { as: 'script', foreignKey: 'script_id' });
    Script.hasMany(ScriptReference, { as: 'references' });

    return sequelize;
  };
