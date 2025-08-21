import { Query as ExpressQuery } from 'express-serve-static-core';
import { Response } from 'express';
import bind from 'utils/monad/either/bind/bind';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import lift from 'utils/monad/either/lift/lift';
import { Either } from 'tsmonad';
import { AppRequest } from 'web/serverModules/types';
import { Context as CardanoContext } from './../../../context/context.types';
import { ScriptReferenceBody } from './createScriptReference.types';
import { AppError } from 'common/error';
import { Fcn } from 'common/types';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import { ScriptReferenceItems, ScriptReferenceRequired } from 'model/sequelize/model/scriptReference/scriptReference.types';
import { RequestImplicits } from '../../../paramHandlers/paramHandlers.types';
import { ScriptReferenceService } from 'service/sequelize/scriptReferenceService/scriptReferenceService.types';

export default (
  bodyValidator: Fcn<[ScriptReferenceBody], Either<AppError, ScriptReferenceBody>>,
  scriptReferenceFactory: Fcn<[ScriptReferenceRequired], Either<AppError, ScriptReferenceItems>>,
  sanitizeEntity: Fcn<[ScriptReference], any>
) =>
  ({ createScriptReference }: ScriptReferenceService) =>
    async (_ctx: CardanoContext, req: AppRequest<unknown, RequestImplicits, ExpressQuery, ScriptReferenceBody>, res: Response): Promise<Either<AppError, ScriptReference>> =>
      Promise.resolve(Either.right<AppError, ScriptReferenceBody>(req.body))
        .then(bind(bodyValidator))
        .then(bind(scriptReferenceFactory))
        .then(asyncBind(createScriptReference()))
        .then(lift((scriptReference: ScriptReference): ScriptReference => res.status(201) && sanitizeEntity(scriptReference)));
