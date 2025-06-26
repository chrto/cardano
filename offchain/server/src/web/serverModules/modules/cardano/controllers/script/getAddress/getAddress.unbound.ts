import requiredProperties from 'utils/object/validator/required/requiredProperties';
import validateProperties, { check } from 'utils/object/validator/properties/properties';
import { scriptTypeFactory } from 'model/cardano/factories';
import { Validator } from 'utils/object/validator/properties/properties.types';
import { Query } from './getAddress.types';
import { Response } from 'express';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { AppRequest } from 'web/serverModules/types';
import { Context as CardanoContext } from '../../../context/context.types';
import { CardanoService } from 'service/cardano/lucid/cardanoService.types';
import { Script } from 'model/cardano/script/script.types';
import { ScriptTypeExt, PlutusVersion, ScriptType } from 'model/cardano/cardano.types';
import { isEnum, isMissing } from 'utils/validation';
import bind from 'utils/monad/either/bind/bind';
import { AddressResponse } from '../../../response/response.types';
import lift from 'utils/monad/either/lift/lift';
import { addressSerializer } from '../../../response/serializers';

const REQUIRED_FIELDS = ['type', 'script'];

const PARAM_CHECK: Validator<Query>[] = [
  check(query => !isMissing(query.type), 'Missing mandatory query parameter: type!'),
  check(query => isEnum(PlutusVersion)(query.type) || isEnum(ScriptTypeExt)(query.type), `Not valid Script Type!`),
  check(query => !isMissing(query.script), 'Missing mandatory query parameter: script!')
];

const queryValidator = (query: Query): Either<AppError, Query> =>
  Either.right<AppError, Query>(query)
    .bind(requiredProperties(REQUIRED_FIELDS))
    .bind(validateProperties(PARAM_CHECK));

const queryToScript = (query: Query): Either<AppError, Script> =>
  scriptTypeFactory(query.type)
    .lift((type: ScriptType) => ({
      type,
      script: query.script
    }));

export default () =>
  ({ getValidatorAddress }: CardanoService) =>
    async (_ctx: CardanoContext, req: AppRequest<unknown, unknown, Query>, _res: Response): Promise<Either<AppError, AddressResponse>> =>
      Promise.resolve(req.query)
        .then(queryValidator)
        .then(bind(queryToScript))
        .then(bind(getValidatorAddress))
        .then(lift(addressSerializer));
