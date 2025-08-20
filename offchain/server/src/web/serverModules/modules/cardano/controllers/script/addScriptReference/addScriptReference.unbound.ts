import { Query as ExpressQuery } from 'express-serve-static-core';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { Response } from 'express';
import { AppRequest } from 'web/serverModules/types';
import { Context as CardanoContext } from './../../../context/context.types';
import { Fcn } from 'common/types';
import { Model } from 'sequelize/types';
import { RequestImplicits } from '../../../paramHandlers/paramHandlers.types';
import { ScriptReferenceBody } from './addScriptReference.types';
import { ScriptReferenceItems, ScriptReferenceRequired } from 'model/sequelize/model/scriptReference/scriptReference.types';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import { ScriptService } from 'service/sequelize/scriptService/scriptService.types';
import bind from 'utils/monad/either/bind/bind';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import lift from 'utils/monad/either/lift/lift';
import { SequelizeService } from 'service/sequelize/types';
import { Script } from 'model/sequelize/model/script/scirpt';

const addReference = (script: Script, service: SequelizeService<[Script, ScriptReferenceItems], ScriptReference>) =>
  (items: ScriptReferenceItems) =>
    service()(script, items);

export default (
  bodyValidator: Fcn<[ScriptReferenceBody], Either<AppError, ScriptReferenceBody>>,
  scriptReferenceFactory: Fcn<[ScriptReferenceRequired], Either<AppError, ScriptReferenceItems>>,
  sanitizeEntity: Fcn<[Model<ScriptReference>], any>
) =>
  ({ createScriptReference }: ScriptService) =>
    async (ctx: CardanoContext, req: AppRequest<unknown, RequestImplicits, ExpressQuery>, res: Response): Promise<Either<AppError, ScriptReference>> =>
      Promise.resolve(Either.right<AppError, ScriptReferenceBody>(req.body))
        .then(bind(bodyValidator))
        .then(lift((body: ScriptReferenceBody): ScriptReferenceRequired => ({
          scriptId: null,
          address: body.address,
          txId: body.txId,
          txIndex: body.txIndex
        })))
        .then(bind(scriptReferenceFactory))
        .then(asyncBind(addReference(ctx.implicits.script, createScriptReference)))
        .then(lift((scriptReference: ScriptReference): ScriptReference => res.status(201) && sanitizeEntity(scriptReference)));
