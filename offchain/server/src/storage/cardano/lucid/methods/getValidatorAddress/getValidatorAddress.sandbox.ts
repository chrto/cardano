
import getValidatorAddress from './getValidatorAddress';
import { Either } from 'tsmonad';
import bind from 'utils/monad/either/bind/bind';
import lucidConfigLoader from 'web/server/configuration/loader/lucid/lucidConfig';
import lift from 'utils/monad/either/lift/lift';
import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { ILucidConfig } from 'web/server/configuration/loader/lucid/lucidConfig.types';
import doer from 'utils/monad/either/do/doer';
import * as sniff from 'supersniff';
import { lucidFactory } from '../../lucid';
import { SpendingValidator as SpendingValidatorModel } from 'model/cardano/script/script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';
import { LucidStorage } from '../../lucid.types';
import { AppError } from 'common/error';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';

require('dotenv').config();

const script: SpendingValidatorModel = {
  type: PlutusVersion.PlutusV2,
  script: '49480100002221200101'
};

Promise.resolve(Either.right<AppError, AppConfig>({} as AppConfig))
  .then(bind(lucidConfigLoader))
  .then(lift((config: AppConfig): ILucidConfig => config.lucid))
  .then(asyncBind(lucidFactory))
  .then(bind((lucid: LucidStorage) => getValidatorAddress(lucid.lucidEvolution)(script)))
  .then(doer({
    right: sniff,
    left: e => sniff(e.message)
  }))
  .catch(sniff);
