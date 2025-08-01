import getAddressUTxOsUnbound from './getAddressUTxOs.unbound';
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
import { Address } from 'model/cardano/cardano.types';
import { DatumType, Transaction } from '../kupoService.types';
import { UTxO } from 'model/cardano/utxo/utxo.types';
import { utxoFactory } from '../kupoFactory';
import { InvalidInput } from 'common/httpErrors';

const CONFIG: ICardanoNodeConfig = {
  kupoUrl: 'http://localhost:1442',
  ogmiosUrl: 'http://localhost:1337'
};

const ADDRESS: Address = 'addr_test1wqag3rt9..2kla';

const AXIOS_RESPONSE: AxiosResponse<Transaction[]> = {
  data: [
    {
      transaction_index: 0,
      transaction_id: 'ae4d985fbaaf803fa05c04ce3c8ca60faf53fd1cde8bf4c70ac953d4f05cee7e',
      output_index: 0,
      address: 'addr_test1wpqlfqjt8czqmd6dqxrxvdr8tp8gt4a8xg40d6jzs9wx7fg93pm8a',
      value: { coins: BigInt(6000000), assets: {} },
      datum_hash: '923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec',
      datum_type: DatumType.inline,
      script_hash: null,
      created_at: {
        slot_no: 84117959,
        header_hash: '87d2e42582179359baaba5ce796b33ddb21cc050c0fa8c53ddb34c9582243160'
      },
      spent_at: {
        slot_no: 84118005,
        header_hash: 'b7da2a6c732c8f2497bd740f541b0c34392da6d5f6a56217efc766ab4cc631ac',
        transaction_id: '6afc79d28cfcbe124c6d74ec93b025d81495a945c070626020bb653c67006be2',
        input_index: 1,
        redeemer: null
      }
    },
    {
      transaction_index: 1,
      transaction_id: '07b9da9c74ea178358db65c7e2ae641e19559fc4365f2f40fa6c0d2412abb8a5',
      output_index: 0,
      address: 'addr_test1wpqlfqjt8czqmd6dqxrxvdr8tp8gt4a8xg40d6jzs9wx7fg93pm8a',
      value: { coins: BigInt(5000000), assets: {} },
      datum_hash: '923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec',
      datum_type: DatumType.inline,
      script_hash: null,
      created_at: {
        slot_no: 84117284,
        header_hash: '31e45e9e6b597b42e735597b233d567704646368dda4be2a1e08c866073dba0e'
      },
      spent_at: null
    }
  ]
} as AxiosResponse<Transaction[]>;

const AXIOS_RESPONSE_ERROR: AxiosResponse<Transaction[]> = {
  data: [
    { transaction_id: '11a73ff956..316f7e28b', transaction_index: 0 } as Transaction,
    { transaction_id: '21a73ff956..316f7e28a', transaction_index: 1 } as Transaction
  ]
} as AxiosResponse<Transaction[]>;

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
      describe(`get Unspend Transactions by Address`, () => {
        let axiosInstance: AxiosInstance = {} as AxiosInstance;
        let axiosStorage: AxiosStorage;
        let result: Either<AppError, UTxO[]>;

        beforeAll(() => {
          appLogger.error = (_) => appLogger; // disable logger
          axiosStorage = axiosUnbound(axiosInstance);
        });

        describe(`Happy path`, () => {
          beforeAll(async () => {
            axiosInstance.request = jest.fn().mockResolvedValue(AXIOS_RESPONSE);

            result = await getAddressUTxOsUnbound
              (axiosStorage.getRequest, requestConfig, sanitizeResponse, utxoFactory)
              (CONFIG)
              (ADDRESS);
          });

          it(`Should retsolved Either with UTxO model list in right side, if everything passed well`, () => {
            result.do({
              right: (utxos: UTxO[]): void => {
                expect(utxos).toBeArrayOfSize(2);
                expect(utxos.map(utxo => utxo.txId)).toIncludeAllMembers(AXIOS_RESPONSE.data.map(utxo => utxo.transaction_id));
              },
              left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
            });
          });
        });

        describe(`Error path`, () => {
          describe(`response data error`, () => {
            beforeAll(async () => {
              axiosInstance.request = jest.fn().mockResolvedValue(AXIOS_RESPONSE_ERROR);
              result = await getAddressUTxOsUnbound
                (axiosStorage.getRequest, requestConfig, sanitizeResponse, utxoFactory)
                (CONFIG)
                (ADDRESS);
            });

            it(`Should resolved Either whit exact AppError in left side, if an error has been thorwn`, () => {
              result.do({
                right: (): void => fail(`Right side has not been expected`),
                left: (error: AppError) => {
                  expect(error).toBeInstanceOf(AppError);
                  expect(error).toBeInstanceOf(InvalidInput);
                  expect(error.message).toEqual(`Kupo UTxO model factory error: Cannot read properties of undefined (reading 'coins')`);
                }
              });
            });
          });

          describe(`Axios request error`, () => {
            beforeAll(async () => {
              axiosInstance.request = jest.fn().mockRejectedValue(ERROR_RESPONSE);
              result = await getAddressUTxOsUnbound
                (axiosStorage.getRequest, requestConfig, sanitizeResponse, utxoFactory)
                (CONFIG)
                (ADDRESS);
            });

            it(`Should resolved Either whit exact AppError in left side, if an error has been thorwn`, () => {
              result.do({
                right: (): void => fail(`Right side has not been expected`),
                left: (error: AppError) => {
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
