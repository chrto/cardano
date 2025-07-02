import { AxiosStorageHandler, HEADER_ACCEPT, HEADERS } from 'storage/http/axios/axios.types';
import { UTxO } from 'model/cardano/utxo/utxo.types';
import { RequestConfig } from 'storage/http/axios/requestConfig/requestConfig.types';
import { AxiosRequestConfig, AxiosResponse } from 'axios';
import { Factory } from 'common/types';
import { QUERY_METCHES, Transaction } from '../kupoService.types';
import { ICardanoNodeConfig } from 'web/server/configuration/loader/cardanoNode/cardanoNodeConfig.types';
import { Address } from 'model/cardano/cardano.types';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import lift from 'utils/monad/either/lift/lift';
import bind from 'utils/monad/either/bind/bind';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import { collectionFactory, getUTxOsByAddressUri } from '../kupoUtils';

export default (
  getRequest: AxiosStorageHandler<Transaction[]>,
  { setHeader, setParams }: RequestConfig,
  sanitizeResponse: (response: AxiosResponse<Transaction[]>) => Transaction[],
  utxoFactory: Factory<Transaction, Either<AppError, UTxO>>
) =>
  (cardanoNodeConfig: ICardanoNodeConfig) =>
    (address: Address): Promise<Either<AppError, UTxO[]>> =>
      Promise.resolve(Either.right<AppError, AxiosRequestConfig>({}))
        .then(lift(setHeader({
          [HEADERS.ACCEPT]: HEADER_ACCEPT.APPL_JSON
        })))
        .then(lift(setParams({
          [QUERY_METCHES.UNSPENT]: null,
          [QUERY_METCHES.RESOLVE_HASHES]: null
        })))
        .then(asyncBind(getRequest(`${cardanoNodeConfig.kupoUrl}${getUTxOsByAddressUri(address)}`)))
        .then(lift(sanitizeResponse))
        .then(bind(collectionFactory<Transaction, UTxO>(utxoFactory)));
