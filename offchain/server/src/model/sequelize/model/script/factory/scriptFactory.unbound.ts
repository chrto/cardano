import { AppError } from 'common/error';
import { Either } from 'tsmonad';
import { ScriptItems, ScriptRequired } from '../script.types';

export default (uuidGenrator: () => string) =>
  (scriptRequired: ScriptRequired): Either<AppError, ScriptItems> =>
    Either.right<AppError, ScriptItems>({
      id: uuidGenrator(),
      ...scriptRequired
    });
