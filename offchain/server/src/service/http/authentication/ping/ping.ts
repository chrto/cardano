import pingUnbound from './ping.unbound';
import axiosStorage from 'storage/http/axios/axios';
import requestConfig from 'storage/http/axios/requestConfig/requestConfig';
import sanitizeResponse from 'storage/http/axios/sanitizeResponse/sanitizeResponse';

export default pingUnbound(axiosStorage.getRequest, requestConfig, sanitizeResponse);
