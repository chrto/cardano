import pluginSdkServiceUnbound from './serviceFactory.unbound';
import cardanoService from 'service/cardano/lucid/cardanoService';
import authenticationService from '../http/authentication/authenticationService';
import userService from '../sequelize/userService/userService';
import scriptService from '../sequelize/scriptService/scriptService';
import cardanoKupoService from 'service/http/kupo/kupoService';

export default pluginSdkServiceUnbound(authenticationService, userService, scriptService, cardanoService, cardanoKupoService);
