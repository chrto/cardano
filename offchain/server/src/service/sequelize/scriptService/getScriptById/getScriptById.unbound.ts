
import { AppError } from 'common/error';
import { NotFound } from 'common/httpErrors';
import { TransactionContext } from 'model/sequelize/modelFactory/modelFactory.types';
import { Script } from 'model/sequelize/model/script/scirpt';
import { SequelizeIncludes } from 'service/sequelize/types';
import { Either } from 'tsmonad';
import { SequelizeStorage } from 'storage/sequelize/factory/sequelizeStorage.types';

export default ({ findByPk }: SequelizeStorage<Script>) =>
  (includes: SequelizeIncludes) =>
    (context?: TransactionContext) =>
      async (id: string): Promise<Either<AppError, Script>> =>
        findByPk(
          id,
          { ...includes, ...context },
          new NotFound(`Cannot find script identified by id = '${id}'`)
        );
