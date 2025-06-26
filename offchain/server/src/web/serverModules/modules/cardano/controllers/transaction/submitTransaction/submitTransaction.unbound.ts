import requiredProperties from 'utils/object/validator/required/requiredProperties';
import validateProperties, { check } from 'utils/object/validator/properties/properties';
import { Body, Query } from './submitTransaction.types';
import { Response } from 'express';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { AppRequest } from 'web/serverModules/types';
import { Context as CardanoContext } from '../../../context/context.types';
import { CardanoService } from 'service/cardano/lucid/cardanoService.types';
import { isMissing } from 'utils/validation';
import { Validator } from 'utils/object/validator/properties/properties.types';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import { CborHex, TxHash } from 'model/cardano/cardano.types';
import { Fcn } from 'common/types';
import bind from 'utils/monad/either/bind/bind';
import { TransactionHashResponse } from '../../../response/response.types';
import lift from 'utils/monad/either/lift/lift';
import { transactionHashSerializer } from '../../../response/serializers';

const REQUIRED_FIELDS = ['transaction'];

const PARAM_CHECK: Validator<Body>[] = [
  check((body: Body) => !isMissing(body.transaction), 'Missing mandatory body parameter: transaction!')
  // ...
];

const bodyValidator = (body: Body): Either<AppError, Body> =>
  Either.right<AppError, Body>(body)
    .bind(requiredProperties(REQUIRED_FIELDS))
    .bind(validateProperties(PARAM_CHECK));

const submitTx = (submitTransaction: Fcn<[CborHex], Promise<Either<AppError, TxHash>>>) => (body: Body): Promise<Either<AppError, TxHash>> =>
  submitTransaction(body.transaction);

export default () =>
  ({ submitTransaction }: CardanoService) =>
    async (_ctx: CardanoContext, req: AppRequest<unknown, unknown, Query, Body>, _res: Response): Promise<Either<AppError, TransactionHashResponse>> =>
      Promise.resolve(Either.right<AppError, Body>(req.body))
        .then(bind(bodyValidator))
        .then(asyncBind(submitTx(submitTransaction)))
        .then(lift(transactionHashSerializer));
