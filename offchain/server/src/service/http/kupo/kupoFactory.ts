import { UTxO } from 'model/cardano/utxo/utxo.types';
import { Datum, DatumType, Transaction } from './kupoService.types';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { InvalidInput } from 'common/httpErrors';
import { Datum as DatumModel } from 'model/cardano/cardano.types';

export const utxoFactory = (tx: Transaction): Either<AppError, UTxO> => {
  try {
    return Either.right<AppError, UTxO>({
      txId: tx.transaction_id,
      txIndex: tx.output_index,

      address: tx.address,
      ...(tx.datum_type === DatumType.inline && !!tx.datum)
        ? { datum: tx.datum }
        : { datumHash: tx.datum_hash },
      scriptHash: tx.script_hash,

      assets: {
        lovelace: tx.value.coins
      }
    });
  } catch (e) {
    return Either.left<AppError, UTxO>(new InvalidInput(`Kupo UTxO model factory error: ${e.message}`));
  }
};

export const datumFactory = (datum: Datum): Either<AppError, DatumModel> =>
  !!datum.datum
    ? Either.right<AppError, DatumModel>(datum.datum)
    : Either.left<AppError, DatumModel>(new InvalidInput('Kupo Datum model factory error: Datum has not been fetched!'));
