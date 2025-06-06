import requiredProperties from 'utils/object/validator/required/requiredProperties';
import validateProperties, { check } from 'utils/object/validator/properties/properties';
import { Body, Query } from './buildTransaction.types';
import bind from 'utils/monad/either/bind/bind';
import { Response } from 'express';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { AppRequest } from 'web/serverModules/types';
import { Context as CardanoContext } from '../../../context/context.types';
import { CardanoService } from 'service/cardano/cardanoService.types';
import { isMissing } from 'utils/validation';
import { Validator } from 'utils/object/validator/properties/properties.types';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import { Address, CborHex, Datum, Lovelace } from 'model/cardano/cardano.types';
import { Fcn } from 'common/types';
import lift from 'utils/monad/either/lift/lift';
import { TransactionCborHexResponse } from '../../../response/response.types';
import { lovelaceDeserializer, transactionCborHexSerializer } from '../../../response/serializers';

const REQUIRED_FIELDS = ['walletAddress', 'contractAddress', 'datum', 'amount'];

const PARAM_CHECK: Validator<Body>[] = [
  check((body: Body) => !isMissing(body.walletAddress), 'Missing mandatory body parameter: walletAddress!'),
  check((body: Body) => !isMissing(body.contractAddress), 'Missing mandatory body parameter: contractAddress!'),
  check((body: Body) => !isMissing(body.datum), 'Missing mandatory body parameter: datum!'),
  check((body: Body) => !isMissing(body.amount), 'Missing mandatory body parameter: amount!')
  // ...
];

const bodyValidator = (body: Body): Either<AppError, Body> =>
  Either.right<AppError, Body>(body)
    .bind(requiredProperties(REQUIRED_FIELDS))
    .bind(validateProperties(PARAM_CHECK));

const buildTx = (buildTransaction: Fcn<[Address, Address, Datum, Lovelace], Promise<Either<AppError, CborHex>>>) => (body: Body): Promise<Either<AppError, CborHex>> =>
  Promise.resolve(lovelaceDeserializer(body.amount))
    .then(asyncBind(amount => buildTransaction(body.walletAddress, body.contractAddress, body.datum, amount)));

export default () =>
  ({ buildTransaction }: CardanoService) =>
    async (_ctx: CardanoContext, req: AppRequest<unknown, unknown, Query, Body>, _res: Response): Promise<Either<AppError, TransactionCborHexResponse>> =>
      Promise.resolve(Either.right<AppError, Body>(req.body))
        .then(bind(bodyValidator))
        .then(asyncBind(buildTx(buildTransaction)))
        .then(lift(transactionCborHexSerializer));
