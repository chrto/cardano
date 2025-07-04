import { AppError } from 'common/error';
import { NotFound } from 'common/httpErrors';
import { Predicate } from 'common/types';
import { NextFunction, Response } from 'express';
import { AppRequest } from 'web/serverModules/types';

export default (isMissing: Predicate<string>) =>
  (_request: AppRequest, _response: Response, next: NextFunction, errMessage?: string) =>
    (error: AppError): void =>
      error instanceof NotFound && !isMissing(errMessage)
        ? next(new NotFound(errMessage))
        : next(error);
