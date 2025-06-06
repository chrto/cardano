import { AppError } from 'common/error';
import { Either } from 'tsmonad';
import { Response } from 'express';
import { AppRequest } from 'web/serverModules/types';
import { Context } from '../../context/context.types';
import { AddressDetailsResponse, CredentialResponse, UTxOResponse } from '../../response/response.types';

export interface AddressController {
  getUTxOs: (ctx: Context, req: AppRequest, res: Response) => Promise<Either<AppError, UTxOResponse[]>>;
  getDetails: (ctx: Context, req: AppRequest, res: Response) => Promise<Either<AppError, AddressDetailsResponse>>;
  getCredentialPayment: (ctx: Context, req: AppRequest, res: Response) => Promise<Either<AppError, CredentialResponse>>;
}
