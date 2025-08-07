import { Association, DataTypes, HasManyAddAssociationMixin, HasManyCountAssociationsMixin, HasManyCreateAssociationMixin, HasManyGetAssociationsMixin, HasManyHasAssociationMixin, HasManyRemoveAssociationsMixin, Model, Sequelize } from 'sequelize';
import { ScriptItems, ScritpCategory } from './script.types';
import { PlutusVersion, ScriptType, ScriptTypeExt } from 'model/cardano/cardano.types';
import { ScriptReference } from '../scriptReference/scriptReference';

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

  public getScriptReferences: HasManyGetAssociationsMixin<ScriptReference>;
  public addScriptReference: HasManyAddAssociationMixin<ScriptReference, string>;
  public hasScriptReference: HasManyHasAssociationMixin<ScriptReference, string>;
  public countScriptReference: HasManyCountAssociationsMixin;
  public createScriptReference: HasManyCreateAssociationMixin<ScriptReference>;
  public deleteScriptReference: HasManyRemoveAssociationsMixin<ScriptReference, string>;
  public references?: ScriptReference[];

  public static associations: {
    references: Association<Script, ScriptReference>;
  };
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
        allowNull: false,
        unique: 'uq_scripts_script'
      },
      category: {
        type: DataTypes.STRING(15),
        allowNull: false,
        validate: {
          isIn: [[ScritpCategory.Burn, ScritpCategory.Gift, ScritpCategory.FortyTwo, ScritpCategory.Vesting, ScritpCategory.Unknown]]
        }
      },
      title: {
        type: DataTypes.STRING(25),
        allowNull: false,
        unique: 'uq_scripts_title'
      },
      description: {
        type: DataTypes.STRING(1000),
        allowNull: true
      },
      createdAt: {
        type: DataTypes.DATE,
        allowNull: false,
        field: 'created_at',
        defaultValue: new Date()
      },
      updatedAt: {
        type: DataTypes.DATE,
        allowNull: false,
        field: 'updated_at',
        defaultValue: new Date()
      }
    },
    {
      sequelize,
      tableName: 'scripts',
      modelName: 'script'
    }
  );
