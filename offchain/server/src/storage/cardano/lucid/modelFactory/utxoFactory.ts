import { Script, UTxO } from '@lucid-evolution/lucid';
import { UTxO as UTxOModel } from 'model/cardano/utxo/utxo.types';
import { isMissing } from 'utils/validation';
import scriptFactory, { toScript } from './scriptFactory';
import valueOrNothing from 'utils/monad/maybe/patterns/valueOrNothing/valueOrNothing';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { Script as ScriptModel } from 'model/cardano/script/script.types';
import toMaybe from 'utils/monad/either/toMaybe/toMaybe';
import { toAddress } from './typesFactory';

export const buildLucid = (utxo: UTxOModel) => (scriptRef: Script = null): UTxO => ({
  txHash: utxo.txId,
  outputIndex: utxo.txIndex,
  address: toAddress(utxo.address),
  assets: utxo.assets,
  datumHash: utxo.datumHash || null,
  datum: utxo.datum || null,
  scriptRef: scriptRef
});

export const toUTxO = (utxo: UTxOModel): UTxO =>
  valueOrNothing<ScriptModel>(utxo.scriptRef)
    .lift<Either<AppError, Script>>(toScript)
    .bind<Script>(toMaybe)
    .caseOf({
      just: buildLucid(utxo),
      nothing: buildLucid(utxo)
    });

const buildModel = (utxo: UTxO) => (scriptRef: ScriptModel = null): UTxOModel => ({
  txId: utxo.txHash,
  txIndex: utxo.outputIndex,
  address: utxo.address,
  assets: utxo.assets,
  ... !isMissing(utxo.datumHash) && { datumHash: utxo.datumHash },
  ... !isMissing(utxo.datum) && { datum: utxo.datum },
  ... !isMissing(scriptRef) && { scriptRef },
  scriptRef
});

export default (utxo: UTxO): UTxOModel =>
  valueOrNothing<Script>(utxo.scriptRef)
    .lift<Either<AppError, ScriptModel>>(scriptFactory)
    .bind<ScriptModel>(toMaybe)
    .caseOf({
      just: buildModel(utxo),
      nothing: buildModel(utxo)
    });
