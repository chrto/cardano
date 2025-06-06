import { LucidEvolution, TxSignBuilder, Address, UTxO, Lovelace, CBORHex, Datum } from '@lucid-evolution/lucid';
import { toUTxO } from '../../modelFactory/utxoFactory';
import { collectionFactory, fromCborHex, toAddress, toDatum } from '../../modelFactory/typesFactory';
import doer from 'utils/monad/either/do/doer';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import bind from 'utils/monad/either/bind/bind';
import eitherify from 'utils/monad/either/eitherify/eitherify';
import lift from 'utils/monad/either/lift/lift';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { LucidError } from 'common/httpErrors';
import { UTxO as UTxOModel } from 'model/cardano/utxo/utxo.types';
import { Address as AddressModel, CborHex as CborHexModel, Datum as DatumModel, Lovelace as LovelaceModel } from 'model/cardano/cardano.types';

const toCBORHex = (txSignBuilder: TxSignBuilder): CBORHex => txSignBuilder.toCBOR();

const selectWalletFromUTxOs = (utxos: UTxO[]) => (lucid: LucidEvolution): void =>
  lucid.selectWallet.fromAddress(
    utxos[0].address,
    utxos.filter((utxo: UTxO) => utxo.address === utxos[0].address)
  );

const buildTtransaction = (address: Address, datum: Datum, lovelace: Lovelace) => (lucid: LucidEvolution): Promise<Either<AppError, TxSignBuilder>> =>
  lucid
    .newTx()
    .pay.ToContract(address, { kind: 'inline', value: datum }, { lovelace })
    .complete()
    .then(Either.right<AppError, TxSignBuilder>)
    .catch(err => Either.left<AppError, TxSignBuilder>(new LucidError(err.message)));

export default (lucid: LucidEvolution) => (utxos: UTxOModel[], address: AddressModel, datum: DatumModel, lovelace: LovelaceModel): Promise<Either<AppError, CborHexModel>> =>
  Promise.resolve(Either.right<AppError, LucidEvolution>(lucid))
    .then(doer({
      right: selectWalletFromUTxOs(collectionFactory<UTxOModel, UTxO>(toUTxO)(utxos))
    }))
    .then(asyncBind(buildTtransaction(toAddress(address), toDatum(datum), lovelace)))
    .then(bind(eitherify(toCBORHex)))
    .then(lift(fromCborHex));
