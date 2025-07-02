import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import { Response } from 'express';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { AppRequest } from 'web/serverModules/types';
import { Context as CardanoContext } from '../../../context/context.types';
import { Address } from 'model/cardano/cardano.types';
import lift from 'utils/monad/either/lift/lift';
import { UTxOResponse } from '../../../response/response.types';
import { collectionSerializer, utxoSerializer } from '../../../response/serializers';
import { CardanoKupoService } from 'service/http/kupo/kupoService.types';

export default () =>
  ({ getAddressUTxOs }: CardanoKupoService) =>
    async (ctx: CardanoContext, _req: AppRequest, _res: Response): Promise<Either<AppError, UTxOResponse[]>> =>
      Promise.resolve(Either.right<AppError, Address>(ctx.implicits.address))
        .then(asyncBind(getAddressUTxOs))
        .then(lift(collectionSerializer(utxoSerializer)));
