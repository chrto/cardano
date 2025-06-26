import storageFactory from 'storage/cardano/lucid/lucid';
import * as sniff from 'supersniff';
import { Datum, Lovelace } from 'model/cardano/cardano.types';
import { ILucidConfig } from 'web/server/configuration/loader/lucid/lucidConfig.types';
import buildTransaction from './buildTransaction';
import { CardanoStorage } from 'storage/cardano/lucid/lucid.types';
import doer from 'utils/monad/either/do/doer';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import lucidConfigLoader from 'web/server/configuration/loader/lucid/lucidConfig';

import bind from 'utils/monad/either/bind/bind';
import lift from 'utils/monad/either/lift/lift';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';

require('dotenv').config();

const DATUM_UNIT: Datum = 'd87980';
const AMOUNT: Lovelace = BigInt(8000000);

Promise.resolve(Either.right<AppError, AppConfig>({} as AppConfig))
  .then(bind(lucidConfigLoader))
  .then(lift((config: AppConfig): ILucidConfig => config.lucid))
  .then(asyncBind(storageFactory))
  .then(asyncBind((storage: CardanoStorage) => buildTransaction(storage)(
    process.env.CARDANO_WALLET_ADDRESS,
    process.env.CARDANO_GIFT_ADDRESS,
    DATUM_UNIT,
    AMOUNT
  )))
  .then(doer({
    right: sniff,
    left: e => sniff(`Error left: ${e.message}`)
  }))
  .catch(e => sniff(`Error catch: ${e.message}`));