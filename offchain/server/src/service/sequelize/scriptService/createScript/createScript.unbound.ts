import { AppError } from 'common/error';
import { Script } from 'model/sequelize/model/script/scirpt';
import { Either } from 'tsmonad';
import { ScriptItems } from 'model/sequelize/model/script/script.types';
import { TransactionContext } from 'model/sequelize/modelFactory/modelFactory.types';
import { SequelizeStorage } from 'storage/sequelize/factory/sequelizeStorage.types';

export default ({ create }: SequelizeStorage<Script>) =>
  (context?: TransactionContext) =>
    async (scriptReq: ScriptItems): Promise<Either<AppError, Script>> =>
      create(scriptReq, context);
