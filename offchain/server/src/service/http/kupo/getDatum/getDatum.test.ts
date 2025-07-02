import getDatumUnbound from './getDatum.unbound';
import axiosUnbound from 'storage/http/axios/axios.unbound';
import appLogger from 'logger/appLogger';
import requestConfig from 'storage/http/axios/requestConfig/requestConfig';
import sanitizeResponse from 'storage/http/axios/sanitizeResponse/sanitizeResponse';
import { AxiosError, AxiosInstance, AxiosResponse } from 'axios';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { AxiosStorage } from 'storage/http/axios/axios.types';
import { AxiosErrorData } from 'storage/http/axios/errorHandler/model.types';
import { ICardanoNodeConfig } from 'web/server/configuration/loader/cardanoNode/cardanoNodeConfig.types';
import { DatumHash as DatumHashModel, Datum as DatumModel } from 'model/cardano/cardano.types';
import { Datum } from '../kupoService.types';
import { datumFactory } from '../kupoFactory';
import { InvalidInput } from 'common/httpErrors';

const CONFIG: ICardanoNodeConfig = {
  kupoUrl: 'http://localhost:1442',
  ogmiosUrl: 'http://localhost:1337'
};

const DATUM: string = 'd87980';
const DATUM_HASH: DatumHashModel = '923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec';

const AXIOS_RESPONSE: AxiosResponse<Datum> = {
  data: {
    datum: DATUM
  }
} as AxiosResponse<Datum>;

let ERROR_RESPONSE: AxiosError<AxiosErrorData, any> = { // TODO
  response: {
    statusText: 'status',
    status: 400,
    data: {
      error_description: 'Some error desc..'
    }
  } as AxiosResponse<any>
} as AxiosError;

describe(`service`, () => {
  describe(`http`, () => {
    describe(`Kupo Service`, () => {
      describe(`get Datum by Datum Hash`, () => {
        let axiosInstance: AxiosInstance = {} as AxiosInstance;
        let axiosStorage: AxiosStorage;
        let result: Either<AppError, DatumModel>;

        beforeAll(() => {
          appLogger.error = (_) => appLogger; // disable logger
          axiosStorage = axiosUnbound(axiosInstance);
        });

        describe(`Happy path`, () => {
          beforeAll(async () => {
            axiosInstance.request = jest.fn().mockResolvedValue(AXIOS_RESPONSE);

            result = await getDatumUnbound
              (axiosStorage.getRequest, requestConfig, sanitizeResponse, datumFactory)
              (CONFIG)
              (DATUM_HASH);
          });

          it(`Should retsolved Either with Datum in right side, if everything passed well`, () => {
            result.do({
              right: (datum: DatumModel): void => {
                expect(datum).toBeString;
                expect(datum).toEqual(DATUM);
              },
              left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
            });
          });
        });

        describe(`Error path`, () => {
          describe(`Axios request error`, () => {
            beforeAll(async () => {
              axiosInstance.request = jest.fn().mockRejectedValue(ERROR_RESPONSE);
              result = await getDatumUnbound
                (axiosStorage.getRequest, requestConfig, sanitizeResponse, datumFactory)
                (CONFIG)
                (DATUM);
            });

            it(`Should resolved Either whit exact AppError in left side, if an error has been thorwn`, () => {
              result.do({
                right: (): void => fail(`Right side has not been expected`),
                left: (error: AppError) => {
                  expect(error).toBeInstanceOf(AppError);
                  expect(error).toBeInstanceOf(InvalidInput);
                  expect(error.message).toEqual(ERROR_RESPONSE.response.data.error_description);
                }
              });
            });
          });
        });
      });
    });
  });
});
