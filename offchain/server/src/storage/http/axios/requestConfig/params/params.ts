import { AxiosRequestConfig } from 'axios';
import { isMissing } from 'utils/validation';
import { stringify } from 'qs';

export const paramsSerializer = params => stringify(params, {
  encode: false,
  arrayFormat: 'repeat',
  skipNulls: false
})
  .split('&')
  .map(part => part.replace(/=($|null)$/, '')) // removes '=null', '='
  .join('&');

export default <RP> (urlParams: RP) =>
  (axiosRequestConfig: AxiosRequestConfig): AxiosRequestConfig =>
    isMissing(urlParams)
      ? axiosRequestConfig
      : {
        ...axiosRequestConfig,
        params: {
          ...axiosRequestConfig.params,
          ...urlParams
        },
        paramsSerializer
      };
