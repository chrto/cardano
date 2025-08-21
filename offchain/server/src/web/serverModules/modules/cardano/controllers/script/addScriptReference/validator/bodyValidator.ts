import bodyValidator, { check } from 'utils/object/validator/properties/properties';
import requiredProperties from 'utils/object/validator/required/requiredProperties';
import { AppError } from 'common/error';
import { Either } from 'tsmonad';
import { isMissing } from 'utils/validation';
import { Validator } from 'utils/object/validator/properties/properties.types';
import { ScriptReferenceBody } from '../addScriptReference.types';

const REQUIRED_FIELDS = ['address', 'txId', 'txIndex'];

const BODY_CHECK: Validator<ScriptReferenceBody>[] = [
  check(body => !isMissing(body.address), 'Missing mandatory property address'),
  check(body => !isMissing(body.txId), 'Missing mandatory property txId'),
  check(body => !isMissing(body.txIndex), 'Missing mandatory property txIndex')
];

export default (body: ScriptReferenceBody): Either<AppError, ScriptReferenceBody> =>
  Either.right<AppError, ScriptReferenceBody>(body)
    .bind(requiredProperties(REQUIRED_FIELDS))
    .bind(bodyValidator(BODY_CHECK));
