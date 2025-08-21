import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import sequelizeStorage from './factory/sequelizeStorage';
import { SequelizeStorage } from './factory/sequelizeStorage.types';

const scriptStorage: SequelizeStorage<ScriptReference> = sequelizeStorage(ScriptReference);
export default scriptStorage;
