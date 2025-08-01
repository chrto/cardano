import { Query as ExpressQuery } from 'express-serve-static-core';
import { Response } from 'express'; import lift from 'utils/monad/either/lift/lift';
import { ScriptService } from 'service/sequelize/scriptService/scriptService.types';
import { AppRequest } from 'web/serverModules/types';
import { Context as PortalContext } from './../../../context/context.types';
import { Script as PortalScript } from 'model/sequelize/model/script/scirpt';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { Fcn } from 'common/types';
import { RequestImplicits } from '../../../paramHandlers/paramHandlers.types';

export default (
  sanitizeEntities: Fcn<[PortalScript[]], any[]>
) =>
  ({ getScripts }: ScriptService) =>
    async (_ctx: PortalContext, _req: AppRequest<unknown, RequestImplicits, ExpressQuery>, _res: Response): Promise<Either<AppError, PortalScript[]>> =>
      getScripts()()
        .then(lift(sanitizeEntities));
