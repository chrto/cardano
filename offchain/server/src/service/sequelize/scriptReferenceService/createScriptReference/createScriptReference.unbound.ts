import { AppError } from 'common/error';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import { Either } from 'tsmonad';
import { TransactionContext } from 'model/sequelize/modelFactory/modelFactory.types';
import { SequelizeStorage } from 'storage/sequelize/factory/sequelizeStorage.types';
import { ScriptReferenceRequired } from 'model/sequelize/model/scriptReference/scriptReference.types';

export default ({ create }: SequelizeStorage<ScriptReference>) =>
  (context?: TransactionContext) =>
    async (scriptReferenceReq: ScriptReferenceRequired): Promise<Either<AppError, ScriptReference>> =>
      create(scriptReferenceReq, context);
