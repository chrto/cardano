import getAddressUTxOsUnbound from './getAddressUTxOs.unbound';
import { AxiosRequestConfig, AxiosResponse } from 'axios';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { UTxO } from 'model/cardano/utxo/utxo.types';
import { Address } from 'model/cardano/cardano.types';
import { QUERY_METCHES, Transaction } from '../kupoService.types';
import { ICardanoNodeConfig } from 'web/server/configuration/loader/cardanoNode/cardanoNodeConfig.types';
import { ParsedUrlQueryInput } from 'querystring';
import { RequestConfig } from 'storage/http/axios/requestConfig/requestConfig.types';
import { HEADER_ACCEPT, HEADERS } from 'storage/http/axios/axios.types';

type AsyncExecutor = jest.Mock<Promise<Either<AppError, AxiosResponse<Transaction[]>>>, [AxiosRequestConfig]>;

const CONFIG: ICardanoNodeConfig = {
  kupoUrl: 'http://localhost:1442',
  ogmiosUrl: 'http://localhost:1337'
};

const ADDRESS: Address = 'addr_test1wqag3rt9..2kla';

const AXIOS_RESPONSE: AxiosResponse<Transaction[]> = {
  data: [
    { transaction_id: '91a73ff956..316f7e28b', transaction_index: 0 } as Transaction,
    { transaction_id: '41a73ff956..316f7e28a', transaction_index: 1 } as Transaction
  ]
} as AxiosResponse<Transaction[]>;

const REQ_HEADERS = {
  [HEADERS.ACCEPT]: HEADER_ACCEPT.APPL_JSON
};

const REQ_PARAMS = {
  [QUERY_METCHES.UNSPENT]: null,
  [QUERY_METCHES.RESOLVE_HASHES]: null
};

describe(`service`, () => {
  describe(`http`, () => {
    describe(`Kupo Service`, () => {
      describe(`get Unspend Transactions by Address`, () => {
        let requestExecutor: AsyncExecutor;
        let getRequest: jest.Mock<AsyncExecutor, [string]>;
        let setHeader: jest.Mock<jest.Mock<AxiosRequestConfig, [AxiosRequestConfig]>, [object]>;
        let setParams: jest.Mock<jest.Mock<AxiosRequestConfig, [AxiosRequestConfig]>, [object]>;
        let sanitizeResponse: jest.Mock<Transaction[], [AxiosResponse<Transaction[]>]>;
        let utxoFactory: jest.Mock<Either<AppError, UTxO>, [Transaction]>;

        beforeAll(async () => {
          requestExecutor = jest.fn().mockResolvedValue(Either.right<AppError, AxiosResponse<Transaction[]>>(AXIOS_RESPONSE));
          getRequest = jest.fn().mockReturnValue(requestExecutor);
          setHeader = jest.fn().mockImplementation(<RH extends ParsedUrlQueryInput> (headers: RH) =>
            (config: AxiosRequestConfig): AxiosRequestConfig => ({ ...config, headers: { ...config.headers, ...headers } }));
          setParams = jest.fn().mockImplementation(<RP extends ParsedUrlQueryInput> (params: RP) =>
            (config: AxiosRequestConfig): AxiosRequestConfig => ({ ...config, params: { ...config.params, ...params } }));
          sanitizeResponse = jest.fn().mockImplementation((response: AxiosResponse<Transaction[]>): Transaction[] => response.data);
          utxoFactory = jest.fn().mockImplementation((tx: Transaction): Either<AppError, UTxO> => Either.right({
            txId: tx.transaction_id,
            txIndex: tx.transaction_index
          } as UTxO));

          const requestCfg: RequestConfig = { setHeader, setBody: null, setMethod: null, setParams, setUrl: null };
          await getAddressUTxOsUnbound
            (getRequest, requestCfg, sanitizeResponse, utxoFactory)
            (CONFIG)
            (ADDRESS);
        });

        it('Should set headers in to axios request configuration', () => {
          expect(setHeader)
            .toHaveBeenCalledTimes(1);
          expect(setHeader)
            .toHaveBeenCalledWith(REQ_HEADERS);
        });

        it('Should set query parameters in to axios request configuration', () => {
          expect(setParams)
            .toHaveBeenCalledTimes(1);
          expect(setParams)
            .toHaveBeenCalledWith(REQ_PARAMS);
        });

        it(`Should call get request with exact parameters, after configuration has been setted`, () => {
          expect(getRequest)
            .toHaveBeenCalledTimes(1);
          expect(getRequest)
            .toHaveBeenCalledWith(`${CONFIG.kupoUrl}/matches/${ADDRESS}`);

          expect(requestExecutor)
            .toHaveBeenCalledTimes(1);
          expect(requestExecutor)
            .toHaveBeenCalledWith({ headers: REQ_HEADERS, params: REQ_PARAMS });

          expect(getRequest)
            .toHaveBeenCalledAfter(setHeader);
          expect(getRequest)
            .toHaveBeenCalledAfter(setParams);
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
          expect(utxoFactory)
            .toHaveBeenCalledTimes(2);
          expect(utxoFactory)
            .toHaveBeenNthCalledWith(1, AXIOS_RESPONSE.data[0]);
          expect(utxoFactory)
            .toHaveBeenNthCalledWith(2, AXIOS_RESPONSE.data[1]);

          expect(utxoFactory)
            .toHaveBeenCalledAfter(sanitizeResponse);
        });
      });
    });
  });
});
