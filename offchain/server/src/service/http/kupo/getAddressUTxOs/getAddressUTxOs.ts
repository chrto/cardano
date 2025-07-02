import getAddressUTxOsUnbound from './getAddressUTxOs.unbound';
import axiosStorage from 'storage/http/axios/axios';
import requestConfig from 'storage/http/axios/requestConfig/requestConfig';
import sanitizeResponse from 'storage/http/axios/sanitizeResponse/sanitizeResponse';
import { utxoFactory } from '../kupoFactory';

export default getAddressUTxOsUnbound(axiosStorage.getRequest, requestConfig, sanitizeResponse, utxoFactory);
