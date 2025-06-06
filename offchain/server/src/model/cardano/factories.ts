import { AppError } from 'common/error';
import { PlutusVersion, ScriptType, ScriptTypeExt } from './cardano.types';
import { Either } from 'tsmonad';
import { InvalidInput } from 'common/httpErrors';

export const scriptTypeFactory = (type: string): Either<AppError, ScriptType> => {
  switch (type) {
    case 'Native': return Either.right<AppError, ScriptType>(ScriptTypeExt.Native);
    case 'PlutusV1': return Either.right<AppError, ScriptType>(PlutusVersion.PlutusV1);
    case 'PlutusV2': return Either.right<AppError, ScriptType>(PlutusVersion.PlutusV2);
    case 'PlutusV3': return Either.right<AppError, ScriptType>(PlutusVersion.PlutusV3);
    default: return Either.left<AppError, ScriptType>(new InvalidInput(`Unknow Script Type '${type}'`));
  }
};
