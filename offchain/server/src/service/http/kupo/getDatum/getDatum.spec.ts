import getDatumUnbound from './getDatum.unbound';
import { AxiosRequestConfig, AxiosResponse } from 'axios';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { DatumHash, Datum as DatumModel } from 'model/cardano/cardano.types';
import { Datum } from '../kupoService.types';
import { ICardanoNodeConfig } from 'web/server/configuration/loader/cardanoNode/cardanoNodeConfig.types';
import { ParsedUrlQueryInput } from 'querystring';
import { RequestConfig } from 'storage/http/axios/requestConfig/requestConfig.types';
import { HEADER_ACCEPT, HEADERS } from 'storage/http/axios/axios.types';

type AsyncExecutor = jest.Mock<Promise<Either<AppError, AxiosResponse<Datum>>>, [AxiosRequestConfig]>;

const CONFIG: ICardanoNodeConfig = {
  kupoUrl: 'http://localhost:1442',
  ogmiosUrl: 'http://localhost:1337'
};

const DATUM_HASH: DatumHash = '923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec';

const AXIOS_RESPONSE: AxiosResponse<Datum> = {
  data: {
    datum: 'd87980'
  }
} as AxiosResponse<Datum>;

const REQ_HEADERS = {
  [HEADERS.ACCEPT]: HEADER_ACCEPT.APPL_JSON
};

describe(`service`, () => {
  describe(`http`, () => {
    describe(`Kupo Service`, () => {
      describe(`get Datum by Datum Hash`, () => {
        let requestExecutor: AsyncExecutor;
        let getRequest: jest.Mock<AsyncExecutor, [DatumHash]>;
        let setHeader: jest.Mock<jest.Mock<AxiosRequestConfig, [AxiosRequestConfig]>, [object]>;
        let sanitizeResponse: jest.Mock<Datum, [AxiosResponse<Datum>]>;
        let datumFactory: jest.Mock<Either<AppError, DatumModel>, [Datum]>;

        beforeAll(async () => {
          requestExecutor = jest.fn().mockResolvedValue(Either.right<AppError, AxiosResponse<Datum>>(AXIOS_RESPONSE));
          getRequest = jest.fn().mockReturnValue(requestExecutor);
          setHeader = jest.fn().mockImplementation(<RH extends ParsedUrlQueryInput> (headers: RH) =>
            (config: AxiosRequestConfig): AxiosRequestConfig => ({ ...config, headers: { ...config.headers, ...headers } }));
          sanitizeResponse = jest.fn().mockImplementation((response: AxiosResponse<Datum>): Datum => response.data);
          datumFactory = jest.fn().mockImplementation((datum: Datum): Either<AppError, DatumModel> => Either.right(datum.datum));

          const requestCfg: RequestConfig = { setHeader, setBody: null, setMethod: null, setParams: null, setUrl: null };
          await getDatumUnbound
            (getRequest, requestCfg, sanitizeResponse, datumFactory)
            (CONFIG)
            (DATUM_HASH);
        });

        it('Should set headers in to axios request configuration', () => {
          expect(setHeader)
            .toHaveBeenCalledTimes(1);
          expect(setHeader)
            .toHaveBeenCalledWith(REQ_HEADERS);
        });

        it(`Should call get request with exact parameters, after configuration has been setted`, () => {
          expect(getRequest)
            .toHaveBeenCalledTimes(1);
          expect(getRequest)
            .toHaveBeenCalledWith(`${CONFIG.kupoUrl}/datums/${DATUM_HASH}`);

          expect(requestExecutor)
            .toHaveBeenCalledTimes(1);
          expect(requestExecutor)
            .toHaveBeenCalledWith({ headers: REQ_HEADERS });

          expect(getRequest)
            .toHaveBeenCalledAfter(setHeader);
        });

        it(`Should sanitize response, after request has been called`, () => {
          expect(sanitizeResponse)
            .toHaveBeenCalledTimes(1);
          expect(sanitizeResponse)
            .toHaveBeenCalledWith(AXIOS_RESPONSE);
          expect(sanitizeResponse)
            .toHaveBeenCalledAfter(getRequest);
        });

        it(`Should build UTxO model list, after response has been sanitized`, () => {
          expect(datumFactory)
            .toHaveBeenCalledTimes(1);
          expect(datumFactory)
            .toHaveBeenCalledWith(AXIOS_RESPONSE.data);
          expect(datumFactory)
            .toHaveBeenCalledAfter(sanitizeResponse);
        });
      });
    });
  });
});
