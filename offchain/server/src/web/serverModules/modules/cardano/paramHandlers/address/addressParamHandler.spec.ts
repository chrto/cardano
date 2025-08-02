import addressParamHandlerUnbound from './addressParamHandler.unbound';
import { NextFunction, Response } from 'express';
import { Address } from 'model/cardano/cardano.types';
import { AppRequest } from 'web/serverModules/types';
import { RequestImplicits } from '../paramHandlers.types';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';

type AddEntityExecutor = jest.Mock<void, [Address]>;

const ADDRESS: Address = 'addr_test1wq8xga0gmzpcm...0eur8fdz7dvqhldz0lng249f0d';

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Cardano', () => {
      describe('Request parameter handlers', () => {
        describe('Address', () => {
          let addEntityExecutor: AddEntityExecutor;
          let addEntityInToRequestImplicits: jest.Mock<AddEntityExecutor, [AppRequest<unknown, RequestImplicits>, Response, NextFunction, string]>;

          beforeAll(() => {
            addEntityExecutor = jest.fn().mockReturnValue(null);
            addEntityInToRequestImplicits = jest.fn().mockReturnValue(addEntityExecutor);
          });

          describe('Happy path', () => {
            beforeAll(async () => {
              jest.clearAllMocks();
              await addressParamHandlerUnbound
                (addEntityInToRequestImplicits)
                ({} as PluginSdkService)
                (null, null, null, ADDRESS);
            });

            it(`Should handle Either right side branch`, () => {
              expect(addEntityInToRequestImplicits).toHaveBeenCalledTimes(1);
              expect(addEntityInToRequestImplicits).toHaveBeenCalledWith(null, null, null, 'address');
              expect(addEntityExecutor).toHaveBeenCalledTimes(1);
              expect(addEntityExecutor).toHaveBeenCalledWith(ADDRESS);
            });
          });
        });
      });
    });
  });
});
