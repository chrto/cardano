import { Script } from 'model/sequelize/model/script/scirpt';
import { Order, WhereOptions } from 'sequelize';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { TransactionContext } from 'model/sequelize/modelFactory/modelFactory.types';
import { SequelizeIncludes } from 'service/sequelize/types';
import { SequelizeStorage } from 'storage/sequelize/factory/sequelizeStorage.types';

export default ({ findAll }: SequelizeStorage<Script>) =>
  (includes: SequelizeIncludes) =>
    (context?: TransactionContext) =>
      async (where?: WhereOptions, order?: Order): Promise<Either<AppError, Script[]>> =>
        findAll({ where, order, ...includes, ...context });
