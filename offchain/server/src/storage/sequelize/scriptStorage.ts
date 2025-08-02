import sequelizeStorage from './factory/sequelizeStorage';
import { Script } from 'model/sequelize/model/script/scirpt';
import { SequelizeStorage } from './factory/sequelizeStorage.types';

const scriptStorage: SequelizeStorage<Script> = sequelizeStorage(Script);
export default scriptStorage;
