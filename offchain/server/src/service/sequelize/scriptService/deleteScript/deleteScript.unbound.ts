import { Script } from 'model/sequelize/model/script/scirpt';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { SdkTransaction, TransactionContext } from 'model/sequelize/modelFactory/modelFactory.types';
import { SequelizeStorage } from 'storage/sequelize/factory/sequelizeStorage.types';
import { DestroyOptions, Transaction } from 'sequelize';
import bind from 'utils/monad/either/bind/bind';
import makeSure from 'utils/monad/either/makeSure/makeSure';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import { Conflict } from 'common/httpErrors';
import { Fcn } from 'common/types';

const hasNoUnspendRefs = (script: Script): boolean =>
  script.scriptReferences.filter((ref: ScriptReference) => ref.unspend).length === 0;

const remove = (context: TransactionContext, destroy: Fcn<[DestroyOptions], Promise<Either<AppError, number>>>) =>
  async (script: Script): Promise<Either<AppError, number>> =>
    Promise.resolve(Either.right<AppError, Script>(script))
      .then(bind(makeSure(hasNoUnspendRefs, new Conflict('This script has unspend ScriptReferences. Can not be deleted!'))))
      .then(asyncBind((script: Script) => destroy({ where: { id: script.id }, ...context })));

export default ({ destroy }: SequelizeStorage<Script>) =>
  (sdkTransaction: SdkTransaction) =>
    (context?: TransactionContext) =>
      async (script: Script): Promise<Either<AppError, number>> =>
        context && context.transaction
          ? remove(context, destroy)(script)
          : sdkTransaction.begin()
            .then((transaction: Transaction) =>
              remove({ transaction }, destroy)(script)
                .then(sdkTransaction.commitOrRollback(transaction))
                .catch(sdkTransaction.rollback(transaction))
            );
