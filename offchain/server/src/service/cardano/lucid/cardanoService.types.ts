import { AppError } from 'common/error';
import { AddressDetails } from 'model/cardano/addressDetails/addressDetails.types';
import { Address, CborHex, Datum, Lovelace, TxHash } from 'model/cardano/cardano.types';
import { Script } from 'model/cardano/script/script.types';
import { UTxO } from 'model/cardano/utxo/utxo.types';
import { Either } from 'tsmonad';

export interface CardanoService {
  getAddressUTxOs: (address: Address) => Promise<Either<AppError, UTxO[]>>;
  getAddressDetails: (address: Address) => Either<AppError, AddressDetails>;
  getValidatorAddress: (script: Script) => Either<AppError, Address>;
  buildTransaction: (walletAddress: Address, contractAddress: Address, datum: Datum, amount: Lovelace) => Promise<Either<AppError, CborHex>>;
  submitTransaction: (transaction: CborHex) => Promise<Either<AppError, TxHash>>;
}
