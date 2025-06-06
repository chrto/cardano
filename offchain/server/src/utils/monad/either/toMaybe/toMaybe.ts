import { AppError } from 'common/error';
import { Either, Maybe } from 'tsmonad';

export default <T> (valueOrError: Either<AppError, T>): Maybe<T> =>
  valueOrError.caseOf({
    right: (value: T) => Maybe.just<T>(value),
    left: (_: AppError) => Maybe.nothing()
  });
