import transactionControllerFactory from '../transactionController';
import cardanoServiceFactory from 'service/cardano/lucid/cardanoService';
import { CardanoService } from 'service/cardano/lucid/cardanoService.types';
import storageFactory from 'storage/cardano/lucid/lucid';
import * as sniff from 'supersniff';
import { Datum } from 'model/cardano/cardano.types';
import { ILucidConfig } from 'web/server/configuration/loader/lucid/lucidConfig.types';
import doer from 'utils/monad/either/do/doer';
import { AppRequest } from 'web/serverModules/types';
import { Body, Query } from './buildTransaction.types';
import { TransactionController } from '../transactionController.types';
import { LovelaceSerialized } from '../../../response/response.types';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import bind from 'utils/monad/either/bind/bind';
import lift from 'utils/monad/either/lift/lift';
import lucidConfigLoader from '../../../../../../server/configuration/loader/lucid/lucidConfig';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
require('dotenv').config();

const DATUM_UNIT: Datum = 'd87980';
const AMOUNT: LovelaceSerialized = '8000000';

const APP_REQUEST: AppRequest<unknown, unknown, Query, Body> = {
  body: {
    walletAddress: process.env.CARDANO_WALLET_ADDRESS,
    contractAddress: process.env.CARDANO_GIFT_ADDRESS,
    datum: DATUM_UNIT,
    amount: AMOUNT
  },
  query: {}
} as AppRequest<unknown, unknown, Query, Body>;

Promise.resolve(Either.right<AppError, AppConfig>({} as AppConfig))
  .then(bind(lucidConfigLoader))
  .then(lift((config: AppConfig): ILucidConfig => config.lucid))
  .then(asyncBind(storageFactory))
  .then(lift(cardanoServiceFactory))
  .then(lift((cardanoService: CardanoService) => ({ cardanoService })))
  .then(lift(transactionControllerFactory))
  .then(asyncBind((transactionController: TransactionController) => transactionController.buildTransaction({}, APP_REQUEST, null)))
  .then(doer({
    right: sniff,
    left: e => sniff(`Error left: ${e.message}`)
  }))
  .catch(e => sniff(`Error catch: ${e.message}`));