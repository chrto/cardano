import { Query as ExpressQuery } from 'express-serve-static-core';
import { Script as PortalScript } from 'model/sequelize/model/script/scirpt';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { Response } from 'express';
import { AppRequest } from 'web/serverModules/types';
import { Context as CardanoContext } from './../../../context/context.types';
import { Fcn } from 'common/types';
import { Model } from 'sequelize/types';
import { RequestImplicits } from '../../../paramHandlers/paramHandlers.types';

export default (
  sanitizeEntity: Fcn<[Model<PortalScript>], any>
) => async (ctx: CardanoContext, _req: AppRequest<unknown, RequestImplicits, ExpressQuery>, _res: Response): Promise<Either<AppError, PortalScript>> =>
    Either.right<AppError, PortalScript>(ctx.implicits.script)
      .lift(sanitizeEntity);
