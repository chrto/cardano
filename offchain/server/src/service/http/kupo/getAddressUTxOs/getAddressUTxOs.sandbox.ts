import getAddressUTxOs from './getAddressUTxOs';
import doer from 'utils/monad/either/do/doer';
import * as sniff from 'supersniff';
import { ICardanoNodeConfig } from 'web/server/configuration/loader/cardanoNode/cardanoNodeConfig.types';

require('dotenv').config();

const ADDRESS = process.env.CARDANO_FORTYTWO_ADDRESS;

const CONFIG: ICardanoNodeConfig = {
  kupoUrl: process.env.PROVIDER_NODE_KUPO_URL,
  ogmiosUrl: process.env.PROVIDER_NODE_OGMIOS_URL
};

getAddressUTxOs
  (CONFIG)
  (ADDRESS)
  .then(doer({
    right: sniff,
    left: sniff
  }));
