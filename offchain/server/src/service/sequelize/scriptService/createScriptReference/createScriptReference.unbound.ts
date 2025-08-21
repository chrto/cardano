
import { AppError } from 'common/error';
import { TransactionContext } from 'model/sequelize/modelFactory/modelFactory.types';
import { Script } from 'model/sequelize/model/script/scirpt';
import { SequelizeIncludes } from 'service/sequelize/types';
import { Either } from 'tsmonad';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import { ScriptReferenceItems } from 'model/sequelize/model/scriptReference/scriptReference.types';
import { Fcn } from 'common/types';

export default (errorHandler: Fcn<[Error, string], AppError>) =>
  (includes: SequelizeIncludes) =>
    (context?: TransactionContext) =>
      async (script: Script, scriptReferenceItems: ScriptReferenceItems): Promise<Either<AppError, ScriptReference>> =>
        script.createScriptReference(scriptReferenceItems, { ...context, ...includes })
          .then(Either.right<AppError, ScriptReference>)
          .catch((error: Error): Either<AppError, ScriptReference> => Either.left<AppError, ScriptReference>(errorHandler(error, `while trying to create ScriptReference:`)));
