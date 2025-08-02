import { NextFunction, Response } from 'express';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { AppRequest } from 'web/serverModules/types';
import { RequestImplicits } from '../paramHandlers.types';
import { Fcn } from 'common/types';
import { Address } from 'model/cardano/cardano.types';

// TODO Validate cardano address will be usefull..
// TODO Load address detail into implicits
export default (
  addEntityInToRequestImplicits: Fcn<[AppRequest<unknown, RequestImplicits>, Response, NextFunction, string], Fcn<[Address], void>>
) =>
  ({ }: PluginSdkService) =>
    async (request: AppRequest<unknown, RequestImplicits>, response: Response, next: NextFunction, address: Address): Promise<void> =>
      Promise.resolve(address)
        .then(addEntityInToRequestImplicits(request, response, next, 'address'))
        .catch(next);
