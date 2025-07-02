import getDatum from './getDatum';
import doer from 'utils/monad/either/do/doer';
import * as sniff from 'supersniff';
import { ICardanoNodeConfig } from 'web/server/configuration/loader/cardanoNode/cardanoNodeConfig.types';

require('dotenv').config();

const DATUM = process.env.DATUM_HASH;

const CONFIG: ICardanoNodeConfig = {
  kupoUrl: process.env.PROVIDER_NODE_KUPO_URL,
  ogmiosUrl: process.env.PROVIDER_NODE_OGMIOS_URL
};

getDatum
  (CONFIG)
  (DATUM)
  .then(doer({
    right: sniff,
    left: sniff
  }));
