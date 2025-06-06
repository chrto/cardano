import caseOf from 'utils/monad/either/caseOf/caseOf';
import { Either } from 'tsmonad';
import { NextFunction, Response } from 'express';
import { AppError } from 'common/error';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { AppRequest } from 'web/serverModules/types';
import { RequestImplicits } from '../paramHandlers.types';
import { Fcn } from 'common/types';
import { Address } from 'model/cardano/cardano.types';

// TODO Validate cardano address will be usefull..
// TODO Load address detail into implicits
export default (
  addEntityInToRequestImplicits: Fcn<[AppRequest<unknown, RequestImplicits>, Response, NextFunction, string], Fcn<[Address], void>>,
  handleError: Fcn<[AppRequest<unknown, RequestImplicits>, Response, NextFunction, string], Fcn<[AppError], void>>
) =>
  ({ }: PluginSdkService) =>
    async (request: AppRequest<unknown, RequestImplicits>, response: Response, next: NextFunction, address: Address): Promise<void> =>
      Promise.resolve(Either.right<AppError, string>(address))
        .then(caseOf({
          right: addEntityInToRequestImplicits(request, response, next, 'address'),
          left: handleError(request, response, next, `There are no UTxOs at address ${address}.`)
        }))
        .catch(next);
