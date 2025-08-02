import { Sequelize } from 'sequelize';
import { InitModels } from './sequelizeInit.types';

export default (initModel: InitModels) =>
  (sequelize: Sequelize): Sequelize => {
    initModel.userModel(sequelize);
    initModel.scriptModel(sequelize);

    return sequelize;
  };
