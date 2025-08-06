import { AppError } from 'common/error';
import { Either } from 'tsmonad';
import { ScriptReferenceItems, ScriptReferenceRequired } from '../scriptReference.types';

export default (uuidGenrator: () => string) =>
  (scriptReferenceRequired: ScriptReferenceRequired): Either<AppError, ScriptReferenceItems> =>
    Either.right<AppError, ScriptReferenceItems>({
      id: uuidGenrator(),
      ...scriptReferenceRequired
    });
