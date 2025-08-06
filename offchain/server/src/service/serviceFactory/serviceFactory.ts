import pluginSdkServiceUnbound from './serviceFactory.unbound';
import cardanoService from 'service/cardano/lucid/cardanoService';
import authenticationService from '../http/authentication/authenticationService';
import userService from '../sequelize/userService/userService';
import scriptService from '../sequelize/scriptService/scriptService';
import scriptReferenceService from '../sequelize/scriptReferenceService/scriptReferenceService';
import cardanoKupoService from 'service/http/kupo/kupoService';

export default pluginSdkServiceUnbound(authenticationService, userService, scriptService, scriptReferenceService, cardanoService, cardanoKupoService);
