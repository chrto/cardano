import { AppError } from 'common/error';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import { Either } from 'tsmonad';
import { TransactionContext } from 'model/sequelize/modelFactory/modelFactory.types';
import { SequelizeStorage } from 'storage/sequelize/factory/sequelizeStorage.types';
import { ScriptReferenceItems } from 'model/sequelize/model/scriptReference/scriptReference.types';

export default ({ create }: SequelizeStorage<ScriptReference>) =>
  (context?: TransactionContext) =>
    async (scriptReferenceReq: ScriptReferenceItems): Promise<Either<AppError, ScriptReference>> =>
      create(scriptReferenceReq, context);
