import cardanoService from 'service/cardano/lucid/cardanoService';
import authenticationService from '../http/authentication/authenticationService';
import userService from '../sequelize/userService/userService';
import pluginSdkServiceUnbound from './serviceFactory.unbound';

export default pluginSdkServiceUnbound(authenticationService, userService, cardanoService);
