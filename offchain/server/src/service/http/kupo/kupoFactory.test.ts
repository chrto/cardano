import { UTxO } from 'model/cardano/utxo/utxo.types';
import { Datum as DatumModel } from 'model/cardano/cardano.types';
import { Datum, DatumType, Transaction } from './kupoService.types';
import { datumFactory, utxoFactory } from './kupoFactory';
import { AppError } from 'common/error';

describe(`service`, () => {
  describe(`http`, () => {
    describe(`Kupo Service`, () => {
      describe(`Factories`, () => {
        describe(`UTxO`, () => {
          it(`Should create UTxO model with inline datum resolved.`, () => {
            const TRANSACTION: Transaction = {
              transaction_index: 1,
              transaction_id: '07b9da9c..fa6c0d2412abb8a5',
              output_index: 0,
              address: 'addr_test1wpq..fg93pm8a',
              value: {
                coins: BigInt(6000000),
                assets: {}
              },
              datum_hash: '923918e403..86f44ec',
              datum: 'd87980',
              datum_type: DatumType.inline,
              script_hash: null,
              script: null,
              created_at: {
                slot_no: 84947284,
                header_hash: '31e45e9e6b5..866073dba0e'
              },
              spent_at: null
            };

            const EXPECTED: UTxO = {
              txId: TRANSACTION.transaction_id,
              txIndex: TRANSACTION.output_index,
              address: TRANSACTION.address,
              datum: 'd87980',
              scriptHash: null,
              assets: { lovelace: BigInt(6000000) }
            };

            utxoFactory(TRANSACTION)
              .do({
                right: (utxo: UTxO) => {
                  expect(utxo).toStrictEqual(EXPECTED);
                },
                left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
              });
          });

          it(`Should create UTxO model without inline datum resolved.`, () => {
            const TRANSACTION: Transaction = {
              transaction_index: 1,
              transaction_id: '07b9da9c..fa6c0d2412abb8a5',
              output_index: 0,
              address: 'addr_test1wpq..fg93pm8a',
              value: {
                coins: BigInt(6000000),
                assets: {}
              },
              datum_hash: '923918e403..86f44ec',
              datum_type: DatumType.inline,
              script_hash: null,
              script: null,
              created_at: {
                slot_no: 84947284,
                header_hash: '31e45e9e6b5..866073dba0e'
              },
              spent_at: null
            };

            const EXPECTED: UTxO = {
              txId: TRANSACTION.transaction_id,
              txIndex: TRANSACTION.output_index,
              address: TRANSACTION.address,
              datumHash: TRANSACTION.datum_hash,
              scriptHash: null,
              assets: { lovelace: BigInt(6000000) }
            };

            utxoFactory(TRANSACTION)
              .do({
                right: (utxo: UTxO) => {
                  expect(utxo).toStrictEqual(EXPECTED);
                },
                left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
              });
          });

          it(`Should create UTxO model with datum hash.`, () => {
            const TRANSACTION: Transaction = {
              transaction_index: 1,
              transaction_id: '07b9da9c..fa6c0d2412abb8a5',
              output_index: 0,
              address: 'addr_test1wpq..fg93pm8a',
              value: {
                coins: BigInt(6000000),
                assets: {}
              },
              datum_hash: '923918e403..86f44ec',
              datum: 'd87980',
              datum_type: DatumType.hash,
              script_hash: null,
              script: null,
              created_at: {
                slot_no: 84947284,
                header_hash: '31e45e9e6b5..866073dba0e'
              },
              spent_at: null
            };

            const EXPECTED: UTxO = {
              txId: TRANSACTION.transaction_id,
              txIndex: TRANSACTION.output_index,
              address: TRANSACTION.address,
              datumHash: TRANSACTION.datum_hash,
              scriptHash: null,
              assets: { lovelace: BigInt(6000000) }
            };

            utxoFactory(TRANSACTION)
              .do({
                right: (utxo: UTxO) => {
                  expect(utxo).toStrictEqual(EXPECTED);
                },
                left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
              });
          });
        });

        describe(`Datum`, () => {
          it(`Should create Datum model.`, () => {
            const DATUM: Datum = {
              datum: 'd87980'
            };

            const EXPECTED: DatumModel = 'd87980';

            datumFactory(DATUM)
              .do({
                right: (datum: DatumModel) => {
                  expect(datum).toEqual(EXPECTED);
                },
                left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
              });
          });
        });
      });
    });
  });
});
