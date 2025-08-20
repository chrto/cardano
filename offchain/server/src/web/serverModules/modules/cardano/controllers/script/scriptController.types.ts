import { Query as ExpressQuery } from 'express-serve-static-core';
import { Response } from 'express';
import { AppError } from 'common/error';
import { Either } from 'tsmonad';
import { AppRequest } from 'web/serverModules/types';
import { Context } from '../../context/context.types';
import { Query } from './getAddress/getAddress.types';
import { AddressResponse } from '../../response/response.types';
import { RequestImplicits } from '../../paramHandlers/paramHandlers.types';
import { ScriptBody as CreateScriptBody } from './createScript/createScript.types';
import { ScriptReferenceBody as AddScriptReferenceBody } from './addScriptReference/addScriptReference.types';
import { Script } from 'model/sequelize/model/script/scirpt';
import { ScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';
import { DeletedScript } from './deleteScript/deleteScript.types';

export interface ScriptController {
  // lucid
  getScirptAddress: (ctx: Context, req: AppRequest<unknown, unknown, Query & ExpressQuery>, res: Response) => Promise<Either<AppError, AddressResponse>>;

  // db access
  getScriptById: (ctx: Context, req: AppRequest<unknown, RequestImplicits, ExpressQuery>, res: Response) => Promise<Either<AppError, Script>>;
  getScripts: (ctx: Context, req: AppRequest<unknown, RequestImplicits, ExpressQuery>, res: Response) => Promise<Either<AppError, Script[]>>;
  createScript: (ctx: Context, req: AppRequest<unknown, RequestImplicits, ExpressQuery, CreateScriptBody>, res: Response) => Promise<Either<AppError, Script>>;
  deleteScript: (ctx: Context, req: AppRequest<unknown, RequestImplicits, ExpressQuery>, res: Response) => Promise<Either<AppError, DeletedScript>>;
  addScriptReference: (ctx: Context, req: AppRequest<unknown, RequestImplicits, ExpressQuery, AddScriptReferenceBody>, res: Response) => Promise<Either<AppError, ScriptReference>>;
}
