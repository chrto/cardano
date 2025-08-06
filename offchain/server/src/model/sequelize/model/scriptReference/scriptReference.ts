import { DataTypes, Model, Sequelize } from 'sequelize';
import { ScriptReferenceItems } from './scriptReference.types';
import { TxHash, TxIndex } from 'model/cardano/cardano.types';
import { Script } from '../script/scirpt';

export class ScriptReference extends Model implements ScriptReferenceItems {
  public readonly id!: string;

  public readonly scriptId!: string;

  public readonly address!: string;
  public readonly txId!: TxHash;
  public readonly txIndex!: TxIndex;

  // timestamps!
  public readonly createdAt!: Date;
  public readonly updatedAt!: Date;
}

export default (sequelize: Sequelize): typeof ScriptReference =>
  ScriptReference.init(
    {
      id: {
        allowNull: false,
        primaryKey: true,
        type: DataTypes.UUID
      },
      scriptId: {
        type: DataTypes.UUID,
        field: 'script_id',
        allowNull: false,
        references: {
          model: Script,
          key: 'id'
        }
      },
      address: {
        type: DataTypes.STRING(120),
        allowNull: false,
      },
      txId: {
        type: DataTypes.STRING(64),
        field: 'tx_id',
        allowNull: false
      },
      txIndex: {
        type: DataTypes.INTEGER,
        field: 'tx_index',
        allowNull: false
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
      tableName: 'script_references',
      modelName: 'scriptReference'
    }
  );
