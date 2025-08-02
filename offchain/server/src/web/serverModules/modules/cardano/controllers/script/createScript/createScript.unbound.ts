import { Query as ExpressQuery } from 'express-serve-static-core';
import { Response } from 'express';
import bind from 'utils/monad/either/bind/bind';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import lift from 'utils/monad/either/lift/lift';
import { Either } from 'tsmonad';
import { ScriptService } from 'service/sequelize/scriptService/scriptService.types';
import { AppRequest } from 'web/serverModules/types';
import { Context as CardanoContext } from './../../../context/context.types';
import { ScriptBody } from './createScript.types';
import { Script as PortalScript } from 'model/sequelize/model/script/scirpt';
import { AppError } from 'common/error';
import { Fcn } from 'common/types';
import { ScriptItems, ScriptRequired } from 'model/sequelize/model/script/script.types';
import { RequestImplicits } from '../../../paramHandlers/paramHandlers.types';

export default (
  bodyValidator: Fcn<[ScriptBody], Either<AppError, ScriptBody>>,
  scriptFactory: Fcn<[ScriptRequired], Either<AppError, ScriptItems>>,
  sanitizeEntity: Fcn<[PortalScript], any>
) =>
  ({ createScript }: ScriptService) =>
    async (_ctx: CardanoContext, req: AppRequest<unknown, RequestImplicits, ExpressQuery, ScriptBody>, res: Response): Promise<Either<AppError, PortalScript>> =>
      Promise.resolve(Either.right<AppError, ScriptBody>(req.body))
        .then(bind(bodyValidator))
        .then(bind(scriptFactory))
        .then(asyncBind(createScript()))
        .then(lift((script: PortalScript): PortalScript => res.status(201) && sanitizeEntity(script)));
