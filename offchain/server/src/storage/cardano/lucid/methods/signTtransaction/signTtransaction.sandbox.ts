import buildTtransactionFromAddress from '../buildTtransactionFromAddress/buildTtransactionFromAddress';
import signTtransaction from './signTtransaction';
import { Constr, Data } from '@lucid-evolution/lucid';
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
import tap from 'utils/monad/either/tap/tap';
import { LucidStorage } from '../../lucid.types';
import { AppError } from 'common/error';

require('dotenv').config();

Promise.resolve(Either.right<AppError, AppConfig>({} as AppConfig))
  .then(bind(lucidConfigLoader))
  .then(lift((config: AppConfig): ILucidConfig => config.lucid))
  .then(asyncBind(lucidFactory))
  .then(asyncBind((lucid: LucidStorage) =>
    buildTtransactionFromAddress(lucid.lucidEvolution)(process.env.CARDANO_WALLET_ADDRESS, process.env.CARDANO_GIFT_ADDRESS, Data.to(new Constr(0, [])), BigInt('10000000'))
      .then(tap(sniff))
      .then(asyncBind(cbor => signTtransaction(lucid.lucidEvolution)(cbor, process.env.CARDANO_PRIVATE_KEY)))
  ))
  .then(doer({
    right: sniff,
    left: e => sniff(`Error: ${e.message}`)
  }))
  .catch(sniff);