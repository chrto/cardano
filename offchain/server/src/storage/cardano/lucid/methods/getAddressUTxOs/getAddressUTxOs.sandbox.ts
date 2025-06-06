
import getAddressUTxOs from './getAddressUTxOs';
import { Either } from 'tsmonad';
import bind from 'utils/monad/either/bind/bind';
import lucidConfigLoader from 'web/server/configuration/loader/lucid/lucidConfig';
import lift from 'utils/monad/either/lift/lift';
import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { ILucidConfig } from 'web/server/configuration/loader/lucid/lucidConfig.types';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import doer from 'utils/monad/either/do/doer';
import * as sniff from 'supersniff';
import { lucidFactory } from '../../lucid';
import { LucidStorage } from '../../lucid.types';
import { AppError } from 'common/error';

require('dotenv').config();

Promise.resolve(Either.right<AppError, AppConfig>({} as AppConfig))
  .then(bind(lucidConfigLoader))
  .then(lift((config: AppConfig): ILucidConfig => config.lucid))
  .then(asyncBind(lucidFactory))
  .then(asyncBind((lucid: LucidStorage) => getAddressUTxOs(lucid.lucidEvolution)(process.env.CARDANO_WALLET_ADDRESS)))
  .then(doer({
    right: sniff
  }))
  .then(lift(utxos => `Number of UTxO: ${utxos.length}`))
  .then(doer({
    right: sniff,
    left: e => sniff(e.message)
  }))
  .catch(sniff);
