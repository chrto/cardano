import { AppError } from 'common/error';
import { Either } from 'tsmonad';
import { Response } from 'express';
import { AppRequest } from 'web/serverModules/types';
import { Context } from '../../context/context.types';
import { Query as BuildTxQuery } from './buildTransaction/buildTransaction.types';
import { Body as BuildTxBody } from './buildTransaction/buildTransaction.types';
import { TransactionCborHexResponse, TransactionHashResponse } from '../../response/response.types';

export interface TransactionController {
  buildTransaction: (ctx: Context, req: AppRequest<unknown, unknown, BuildTxQuery, BuildTxBody>, res: Response) => Promise<Either<AppError, TransactionCborHexResponse>>;
  submitTransaction: (ctx: Context, req: AppRequest, res: Response) => Promise<Either<AppError, TransactionHashResponse>>;
}
