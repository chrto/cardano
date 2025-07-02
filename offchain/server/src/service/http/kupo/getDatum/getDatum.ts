import axiosStorage from 'storage/http/axios/axios';
import getDatumUnbound from './getDatum.unbound';
import requestConfig from 'storage/http/axios/requestConfig/requestConfig';
import sanitizeResponse from 'storage/http/axios/sanitizeResponse/sanitizeResponse';
import { datumFactory } from '../kupoFactory';

export default getDatumUnbound(axiosStorage.getRequest, requestConfig, sanitizeResponse, datumFactory);
