import { Query as ExpressQuery } from 'express-serve-static-core';
import { AppError } from 'common/error';
import { Either } from 'tsmonad';
import { Response } from 'express';
import { AppRequest } from 'web/serverModules/types';
import { Context } from '../../context/context.types';
import { Query } from './getAddress/getAddress.types';
import { AddressResponse } from '../../response/response.types';

export interface ScriptController {
  getScirptAddress: (ctx: Context, req: AppRequest<unknown, unknown, Query & ExpressQuery>, res: Response) => Promise<Either<AppError, AddressResponse>>;
}
