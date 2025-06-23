import { Data, Constr } from 'lucid-cardano';
import isInt from '../validators/isInt';

const DEFAULT_UNIT_CBOR = Data.to(new Constr(0, []))

const IntegerRedeemer = Data.Integer();

export const toCBOR = (data) =>
  !data
    ? DEFAULT_UNIT_CBOR
    : isInt(data)
      // eslint-disable-next-line no-undef
      ? Data.to(BigInt(data), IntegerRedeemer)
      : DEFAULT_UNIT_CBOR