import lift from 'utils/monad/either/lift/lift';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { DeletedScript } from './deleteScript.types';
import { AppRequest } from 'web/serverModules/types';
import { RequestImplicits } from '../../../paramHandlers/paramHandlers.types';
import { Context as CardanoContext } from '../../../context/context.types';
import { Response } from 'express';
import { ScriptService } from 'service/sequelize/scriptService/scriptService.types';

export default () =>
  ({ deleteScript }: ScriptService) =>
    async (ctx: CardanoContext, _req: AppRequest<unknown, RequestImplicits>, _res: Response): Promise<Either<AppError, DeletedScript>> =>
      Promise.resolve(Either.right(ctx.implicits.script))
        .then(asyncBind(deleteScript()))
        .then(lift((_: number): DeletedScript => ({ removedScriptId: ctx.implicits.script.id })));
