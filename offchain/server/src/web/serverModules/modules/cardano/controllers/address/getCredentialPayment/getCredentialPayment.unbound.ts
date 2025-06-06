import bind from 'utils/monad/either/bind/bind';
import { Response } from 'express';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { AppRequest } from 'web/serverModules/types';
import { Context as CardanoContext } from '../../../context/context.types';
import { CardanoService } from 'service/cardano/cardanoService.types';
import { Address } from 'model/cardano/cardano.types';
import { Credential } from 'model/cardano/credential/credential.types';
import { AddressDetails } from 'model/cardano/addressDetails/addressDetails.types';
import valueOrError from 'utils/monad/either/patterns/valueOrError/valueOrError';
import { NotFound } from 'common/httpErrors';
import { CredentialResponse } from '../../../response/response.types';

export default () =>
  ({ getAddressDetails }: CardanoService) =>
    async (ctx: CardanoContext, _req: AppRequest, _res: Response): Promise<Either<AppError, CredentialResponse>> =>
      Promise.resolve(Either.right<AppError, Address>(ctx.implicits.address))
        .then(bind(getAddressDetails))
        .then(bind((detail: AddressDetails) => valueOrError<Credential>
          (new NotFound(`Address '${detail.address.bech32}' does not have payment credential!`))
          (detail.paymentCredential)
        ));
