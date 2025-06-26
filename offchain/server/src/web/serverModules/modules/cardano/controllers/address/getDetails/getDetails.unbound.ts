import bind from 'utils/monad/either/bind/bind';
import { Response } from 'express';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { AppRequest } from 'web/serverModules/types';
import { Context as CardanoContext } from '../../../context/context.types';
import { CardanoService } from 'service/cardano/lucid/cardanoService.types';
import { Address } from 'model/cardano/cardano.types';
import { AddressDetailsResponse } from '../../../response/response.types';

export default () =>
  ({ getAddressDetails }: CardanoService) =>
    async (ctx: CardanoContext, _req: AppRequest, _res: Response): Promise<Either<AppError, AddressDetailsResponse>> =>
      Promise.resolve(Either.right<AppError, Address>(ctx.implicits.address))
        .then(bind(getAddressDetails));
