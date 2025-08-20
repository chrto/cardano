import { Query as ExpressQuery } from 'express-serve-static-core';
import { Response } from 'express';
import { AppError } from 'common/error';
import { Either } from 'tsmonad';
import { AppRequest } from 'web/serverModules/types';
import { Context } from '../../context/context.types';
import { RequestImplicits } from '../../paramHandlers/paramHandlers.types';
import { ScriptReferenceBody as CreateScriptReferenceBody } from './createScriptReference/createScriptReference.types';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';

export interface ScriptReferenceController {
  getScriptReferenceById: (ctx: Context, req: AppRequest<unknown, RequestImplicits, ExpressQuery>, res: Response) => Promise<Either<AppError, ScriptReference>>;
  createScriptReference: (ctx: Context, req: AppRequest<unknown, RequestImplicits, ExpressQuery, CreateScriptReferenceBody>, res: Response) => Promise<Either<AppError, ScriptReference>>;
}
