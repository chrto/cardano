import { Query as ExpressQuery } from 'express-serve-static-core';
import { Response } from 'express'; import lift from 'utils/monad/either/lift/lift';
import { ScriptReferenceService } from 'service/sequelize/scriptReferenceService/scriptReferenceService.types';
import { AppRequest } from 'web/serverModules/types';
import { Context as CardanoContext } from './../../../context/context.types';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { Fcn } from 'common/types';
import { RequestImplicits } from '../../../paramHandlers/paramHandlers.types';
import { Query } from './getScriptReferences.types';
import { WhereOptions } from 'sequelize';

export default (
  sanitizeEntities: Fcn<[ScriptReference[]], any[]>,
  filterBuilder: Fcn<[Query], WhereOptions>
) =>
  ({ getScriptReferences }: ScriptReferenceService) =>
    async (_ctx: CardanoContext, req: AppRequest<unknown, RequestImplicits, Query & ExpressQuery>, _res: Response): Promise<Either<AppError, ScriptReference[]>> =>
      Promise.resolve(req.query)
        .then(filterBuilder)
        .then(getScriptReferences())
        .then(lift(sanitizeEntities));
