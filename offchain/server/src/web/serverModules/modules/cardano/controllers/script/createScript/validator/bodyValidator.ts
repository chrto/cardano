import bodyValidator, { check } from 'utils/object/validator/properties/properties';
import requiredProperties from 'utils/object/validator/required/requiredProperties';
import { AppError } from 'common/error';
import { Either } from 'tsmonad';
import { ScriptBody } from '../createScript.types';
import { isEnum, isMissing } from 'utils/validation';
import { Validator } from 'utils/object/validator/properties/properties.types';
import { ScritpCategory } from 'model/sequelize/model/script/script.types';
import { PlutusVersion, ScriptTypeExt } from 'model/cardano/cardano.types';

const REQUIRED_FIELDS = ['type', 'script', 'category', 'title', 'description'];

const BODY_CHECK: Validator<ScriptBody>[] = [
  check(body => !isMissing(body.type), 'Missing mandatory property type'),
  check(body => isMissing(body.type) || isEnum(PlutusVersion)(body.type) || isEnum(ScriptTypeExt)(body.type), 'Invalid script type'),
  check(body => !isMissing(body.script), 'Missing mandatory property script'),
  check(body => !isMissing(body.category), 'Missing mandatory property category'),
  check(body => isMissing(body.category) || isEnum(ScritpCategory)(body.category), 'Invalid script category'),
  check(body => !isMissing(body.title), 'Missing mandatory property title'),
  check(body => !isMissing(body.description), 'Missing mandatory property description')
];

export default (body: ScriptBody): Either<AppError, ScriptBody> =>
  Either.right<AppError, ScriptBody>(body)
    .bind(requiredProperties(REQUIRED_FIELDS))
    .bind(bodyValidator(BODY_CHECK));
