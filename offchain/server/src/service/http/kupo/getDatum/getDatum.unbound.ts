import { AxiosStorageHandler, HEADER_ACCEPT, HEADERS } from 'storage/http/axios/axios.types';
import { RequestConfig } from 'storage/http/axios/requestConfig/requestConfig.types';
import { AxiosRequestConfig, AxiosResponse } from 'axios';
import { Factory } from 'common/types';
import { Datum } from '../kupoService.types';
import { ICardanoNodeConfig } from 'web/server/configuration/loader/cardanoNode/cardanoNodeConfig.types';
import { Datum as DatumModel, DatumHash as DatumHashModel } from 'model/cardano/cardano.types';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import lift from 'utils/monad/either/lift/lift';
import bind from 'utils/monad/either/bind/bind';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import { getDatumByHashUri } from '../kupoUtils';

export default (
  getRequest: AxiosStorageHandler<Datum>,
  { setHeader }: RequestConfig,
  sanitizeResponse: (response: AxiosResponse<Datum>) => Datum,
  datumFactory: Factory<Datum, Either<AppError, DatumModel>>
) =>
  (cardanoNodeConfig: ICardanoNodeConfig) =>
    (datumHash: DatumHashModel): Promise<Either<AppError, DatumModel>> =>
      Promise.resolve(Either.right<AppError, AxiosRequestConfig>({}))
        .then(lift(setHeader({
          [HEADERS.ACCEPT]: HEADER_ACCEPT.APPL_JSON
        })))
        .then(asyncBind(getRequest(`${cardanoNodeConfig.kupoUrl}${getDatumByHashUri(datumHash)}`)))
        .then(lift(sanitizeResponse))
        .then(bind(datumFactory));
