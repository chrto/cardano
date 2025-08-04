import { Script } from 'model/sequelize/model/script/scirpt';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { TransactionContext } from 'model/sequelize/modelFactory/modelFactory.types';
import { SequelizeStorage } from 'storage/sequelize/factory/sequelizeStorage.types';

export default ({ destroy }: SequelizeStorage<Script>) =>
  (context?: TransactionContext) =>
    async (script: Script): Promise<Either<AppError, number>> =>
      destroy({ where: { id: script.id }, ...context });
