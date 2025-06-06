import { LucidEvolution, TxSignBuilder, Address, Lovelace, CBORHex, Datum } from '@lucid-evolution/lucid';
import { fromCborHex, toAddress, toDatum } from '../../modelFactory/typesFactory';
import doer from 'utils/monad/either/do/doer';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import bind from 'utils/monad/either/bind/bind';
import eitherify from 'utils/monad/either/eitherify/eitherify';
import lift from 'utils/monad/either/lift/lift';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { LucidError } from 'common/httpErrors';
import { Address as AddressModel, CborHex as CborHexModel, Datum as DatumModel, Lovelace as LovelaceModel } from 'model/cardano/cardano.types';

const toCBORHex = (txSignBuilder: TxSignBuilder): CBORHex => txSignBuilder.toCBOR();

const selectWalletFromAddress = (address: Address) => (lucid: LucidEvolution): void =>
  lucid.selectWallet.fromAddress(address, []);

const buildTtransaction = (address: Address, datum: Datum, lovelace: Lovelace) => (lucid: LucidEvolution): Promise<Either<AppError, TxSignBuilder>> =>
  lucid
    .newTx()
    .pay.ToContract(address, { kind: 'inline', value: datum }, { lovelace })
    .complete()
    .then(Either.right<AppError, TxSignBuilder>)
    .catch(err => Either.left<AppError, TxSignBuilder>(new LucidError(err.message)));

export default (lucid: LucidEvolution) => (walletAddress: AddressModel, contractAddress: AddressModel, datum: DatumModel, lovelace: LovelaceModel): Promise<Either<AppError, CborHexModel>> =>
  Promise.resolve(Either.right<AppError, LucidEvolution>(lucid))
    .then(doer({
      right: selectWalletFromAddress(toAddress(walletAddress))
    }))
    .then(asyncBind(buildTtransaction(toAddress(contractAddress), toDatum(datum), lovelace)))
    .then(bind(eitherify(toCBORHex)))
    .then(lift(fromCborHex));
