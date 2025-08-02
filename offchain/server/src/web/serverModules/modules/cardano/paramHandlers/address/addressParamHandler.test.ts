import addressParamHandlerUnbound from './addressParamHandler.unbound';
import { NextFunction, Response } from 'express';
import { AppRequest } from 'web/serverModules/types';
import { RequestImplicits } from '../paramHandlers.types';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import addEntityInToRequestImplicits from 'web/serverModules/common/paramHandlers/addEntityInToRequestImplicits/addEntityInToRequestImplicits';
import { Fcn } from 'common/types';
import { Address } from 'model/cardano/cardano.types';

const ADDRESS: Address = 'addr_test1wq8xga0gmzpcm...0eur8fdz7dvqhldz0lng249f0d';

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Cardano', () => {
      describe('Request parameter handlers', () => {
        describe('Address', () => {
          let nextFunction: jest.Mock<void, [any]>;
          let addressParamHandler: Fcn<[AppRequest<unknown, RequestImplicits>, Response, NextFunction, string], Promise<void>>;

          beforeAll(() => {
            nextFunction = jest.fn().mockReturnValue(null);

            addressParamHandler = addressParamHandlerUnbound
              (addEntityInToRequestImplicits)
              ({} as PluginSdkService);
          });

          describe('Happy path', () => {
            let req: AppRequest<unknown, RequestImplicits> = {} as AppRequest<unknown, RequestImplicits>;
            beforeAll(async () => {
              jest.clearAllMocks();
              await addressParamHandler(req, null, nextFunction, ADDRESS);
            });

            it(`Should handle Either right side branch`, () => {
              expect(nextFunction).toHaveBeenCalledTimes(1);
              expect(nextFunction).toHaveBeenCalledWith();
            });

            it(`Should add address in to request implicits`, () => {
              expect(req).toHaveProperty('implicits');
              expect(req.implicits).toHaveProperty('address');
              expect(req.implicits.address).toStrictEqual(ADDRESS);
            });
          });
        });
      });
    });
  });
});
