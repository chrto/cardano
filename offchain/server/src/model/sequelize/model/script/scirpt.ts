import { DataTypes, Model, Sequelize } from 'sequelize';
import { ScriptItems, ScritpCategory } from './script.types';
import { PlutusVersion, ScriptType, ScriptTypeExt } from 'model/cardano/cardano.types';

export class Script extends Model implements ScriptItems {
  public readonly id!: string;

  public readonly type!: ScriptType;
  public readonly script!: string;

  public category: ScritpCategory;
  public title: string;
  public description: string;

  // timestamps!
  public readonly createdAt!: Date;
  public readonly updatedAt!: Date;
}

export default (sequelize: Sequelize): typeof Script =>
  Script.init(
    {
      id: {
        allowNull: false,
        primaryKey: true,
        type: DataTypes.UUID
      },
      type: {
        type: DataTypes.STRING(10),
        allowNull: false,
        defaultValue: PlutusVersion.PlutusV2,
        validate: {
          isIn: [[PlutusVersion.PlutusV1, PlutusVersion.PlutusV2, PlutusVersion.PlutusV3, ScriptTypeExt.Native]]
        }
      },
      script: {
        type: DataTypes.STRING(),
        allowNull: false
      },
      category: {
        type: DataTypes.STRING(15),
        allowNull: false
      },
      title: {
        type: DataTypes.STRING(25),
        allowNull: false
      },
      description: {
        type: DataTypes.STRING(100),
        allowNull: true
      }
    },
    {
      sequelize,
      tableName: 'scripts',
      modelName: 'script'
    }
  );
