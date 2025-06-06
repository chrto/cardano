
import { getAddressDetails as addressDetailsLucid } from '@lucid-evolution/lucid';

import getAddressDetail from './getAddressDetail';
import * as sniff from 'supersniff';

require('dotenv').config();

getAddressDetail(addressDetailsLucid)(process.env.CARDANO_GIFT_ADDRESS)
  .do({
    right: sniff,
    left: e => sniff(e.message)
  });
