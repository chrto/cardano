import { LucidEvolution } from '@lucid-evolution/lucid';
import { AppError } from 'common/error';
import { AddressDetails as AddressDetailsModel } from 'model/cardano/addressDetails/addressDetails.types';
import { Address as AddressModel, Datum as DatumModel, Lovelace as LovelaceModel, CborHex as CborHexModel, TxHash as TxHashModel, PrivateKey as PrivateKeyModel } from 'model/cardano/cardano.types';
import { SpendingValidator as SpendingValidatorModel } from 'model/cardano/script/script.types';
import { UTxO as UTxOModel } from 'model/cardano/utxo/utxo.types';
import { Either } from 'tsmonad';

export interface CardanoStorage {
  getAddressUTxOs: (address: AddressModel) => Promise<Either<AppError, UTxOModel[]>>;
  getValidatorAddress: (validator: SpendingValidatorModel) => Either<AppError, AddressModel>;
  getAddressDetail: (address: AddressModel) => Either<AppError, AddressDetailsModel>;
  buildTransactionFromAddress: (walletAddress: AddressModel, contractAddress: AddressModel, datum: DatumModel, amount: LovelaceModel) => Promise<Either<AppError, CborHexModel>>;
  buildTransactionFromUTxOs: (utxos: UTxOModel[], contractAddress: AddressModel, datum: DatumModel, amount: LovelaceModel) => Promise<Either<AppError, CborHexModel>>;
  submitTransaction: (transaction: CborHexModel, privateKey: PrivateKeyModel) => Promise<Either<AppError, TxHashModel>>;
}

export interface LucidStorage {
  lucidEvolution: LucidEvolution;
}