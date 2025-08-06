
import { AppError } from 'common/error';
import { NotFound } from 'common/httpErrors';
import { TransactionContext } from 'model/sequelize/modelFactory/modelFactory.types';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import { SequelizeIncludes } from 'service/sequelize/types';
import { Either } from 'tsmonad';
import { SequelizeStorage } from 'storage/sequelize/factory/sequelizeStorage.types';

export default ({ findByPk }: SequelizeStorage<ScriptReference>) =>
  (includes: SequelizeIncludes) =>
    (context?: TransactionContext) =>
      async (id: string): Promise<Either<AppError, ScriptReference>> =>
        findByPk(
          id,
          { ...includes, ...context },
          new NotFound(`Cannot find script reference identified by id = '${id}'`)
        );
