import { AppError } from 'common/error';
import { Address, Amount, DatumHash, Datum as DatumModel, Hash, ScriptHash, TxHash, TxIndex } from 'model/cardano/cardano.types';
import { Script } from 'model/cardano/script/script.types';
import { UTxO } from 'model/cardano/utxo/utxo.types';
import { Either } from 'tsmonad';

// https://cardanosolutions.github.io/kupo/#operation/getAllMatches
export enum QUERY_METCHES {
  RESOLVE_HASHES = 'resolve_hashes',
  SPENT = 'spent',
  UNSPENT = 'unspent',
  ORDER = 'order',
  CREATED_AFTER = 'created_after',
  SPENT_AFTER = 'spent_after',
  CREATED_BEFORE = 'created_before',
  SPENT_BEFORE = 'spent_before',
  POLICY_ID = 'policy_id',
  ASSET_NAME = 'asset_name',
  TRANSACTION_ID = 'transaction_id',
  OUTPUT_INDEX = 'output_index'
}

// https://cardanosolutions.github.io/kupo/#operation/getAllMatches
export enum ORDER_MATCHES {
  NEWEST_FIRST = 'most_recent_first',
  OLDEST_FIRST = 'oldest_first'
}
export enum DatumType {
  inline = 'inline',      // means that the ouput originally contained a full inline datum
  hash = 'hash'           // means that the output only contains a reference to the datum
}

export interface ChainPoint {
  slot_no: number;      // An absolut slot number
  header_hash: Hash;    // A blake2b-256 hash digest of a block header
}

export interface TxSpent extends ChainPoint {
  transaction_id: TxHash; // A blake2b-256 hash digest of a transaction body
  input_index: TxIndex;   // The index of the input within the block transaction carrying it.
  redeemer: string;       // A serialized Plutus' Data (datum or redeemer)
}

export interface TxValue {
  coins: Amount;  // A quantity of Lovelace.
  assets: object;    // A key:value map of asset identifier → quantity.
}

export interface Transaction {
  transaction_index: TxIndex; // The index of the transaction within the block including it.
  transaction_id: TxHash;     // A blake2b-256 hash digest of a transaction body.
  output_index: TxIndex;      // The index of the output within the transaction carrying it.
  address: Address;           // A Cardano address, in any era.
  value: TxValue;             // A (multi-asset) value of a transaction's output.
  datum_hash: DatumHash;      // A blake2b-256 hash digest of a Plutus' datum, if any.
  datum?: DatumModel;          // The resolved datum, if available. The field is only and always present (yet may be null) if ?resolve_hashes was set.
  datum_type?: DatumType;     // Categorize the type of datum in the output
  script_hash: ScriptHash;    // A blake2b-224 hash digest of a Native or Plutus script, if any
  script?: Script;            // The resolved script, if available. The field is only and always present (yet may be null) if ?resolve_hashes was set.
  created_at: ChainPoint;     // Block reference at which this transaction was included in the ledger
  spent_at: TxSpent;          // Block reference at which this transaction input was spent, if any
}

export interface Datum {
  datum: DatumModel;    // A serialized Plutus' Data (datum or redeemer)
}

export interface CardanoKupoService {
  getAddressUTxOs: (address: Address) => Promise<Either<AppError, UTxO[]>>;     // unspend transactions
  getDatumByHash: (datum: DatumHash) => Promise<Either<AppError, DatumModel>>;
}
