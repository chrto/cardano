import { AsyncStartStop, SdkTransaction } from 'model/sequelize/modelFactory/modelFactory.types';

import { AuthenticationService } from '../http/authentication/types';
import { UserService } from '../sequelize/userService/userService.types';
import { CardanoService } from 'service/cardano/lucid/cardanoService.types';
import { CardanoKupoService } from 'service/http/kupo/kupoService.types';

export interface PluginSdkService {
  sdkStartStop: AsyncStartStop;
  sdkTransaction: SdkTransaction;
  authenticationService: AuthenticationService;
  userService: UserService;
  cardanoService: CardanoService;
  cardanoKupoService: CardanoKupoService;
}
