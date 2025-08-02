import { Query as ExpressQuery } from 'express-serve-static-core';
import { Response } from 'express'; import lift from 'utils/monad/either/lift/lift';
import { ScriptService } from 'service/sequelize/scriptService/scriptService.types';
import { AppRequest } from 'web/serverModules/types';
import { Context as CardanoContext } from './../../../context/context.types';
import { Script as PortalScript } from 'model/sequelize/model/script/scirpt';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { Fcn } from 'common/types';
import { RequestImplicits } from '../../../paramHandlers/paramHandlers.types';
import { Query } from './getScripts.types';
import { WhereOptions } from 'sequelize';

export default (
  sanitizeEntities: Fcn<[PortalScript[]], any[]>,
  filterBuilder: Fcn<[Query], WhereOptions>
) =>
  ({ getScripts }: ScriptService) =>
    async (_ctx: CardanoContext, req: AppRequest<unknown, RequestImplicits, Query & ExpressQuery>, _res: Response): Promise<Either<AppError, PortalScript[]>> =>
      Promise.resolve(req.query)
        .then(filterBuilder)
        .then(getScripts())
        .then(lift(sanitizeEntities));
