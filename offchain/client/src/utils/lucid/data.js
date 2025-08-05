import { Data, Constr } from 'lucid-cardano';
// import isInt from '../validators/isInt';

const DEFAULT_UNIT_CBOR = Data.to(new Constr(0, []));
const DEFAULT_UNIT_DATA = null;

const VestingDatum = Data.Object({
  beneficiary: Data.Bytes,
  deadline: Data.Integer
})

const IntegerData = Data.Integer();

const getCBORFromData = (data, type) => {
  switch (type) {
    case 'integer':
      try {
        // eslint-disable-next-line no-undef
        return Data.to(BigInt(data), IntegerData)
      } catch (err) {
        console.error(`Can not convert IntegerData to CBOR: ${err}`)
        return null
      }
    case 'vesting':
      try {
        // eslint-disable-next-line no-undef
        return Data.to(data, VestingDatum)
      } catch (err) {
        console.error(`Can not convert VestingDatum to CBOR: ${err}`)
        return null
      }
    default: return DEFAULT_UNIT_CBOR;
  }
}

export const datumToCBOR = (datum) =>
  !datum
    ? DEFAULT_UNIT_CBOR
    : getCBORFromData(datum.data, datum.type)

export const redeemerToCBOR = (redeemer, type) =>
  !redeemer
    ? DEFAULT_UNIT_CBOR
    : getCBORFromData(redeemer, type)

const getDataFromCBOR = (data, type) => {
  switch (type) {
    case 'vesting':
      try {
        return Data.from(data, VestingDatum);
      } catch (err) {
        return null;
      }
    case 'integer':
      try {
        return Data.from(data, IntegerData);
      } catch (err) {
        return null;
      }
    default: return null;
  }
}

export const datumFromCBOR = (datum, type) =>
  !datum
    ? DEFAULT_UNIT_DATA
    : getDataFromCBOR(datum, type)

export const redeemerFromCBOR = (redeemer, type) =>
  !redeemer
    ? DEFAULT_UNIT_DATA
    : getDataFromCBOR(redeemer, type)