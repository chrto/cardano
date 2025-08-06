import { AsyncStartStop, SdkTransaction } from 'model/sequelize/modelFactory/modelFactory.types';

import { AuthenticationService } from '../http/authentication/types';
import { UserService } from '../sequelize/userService/userService.types';
import { ScriptService } from 'service/sequelize/scriptService/scriptService.types';
import { CardanoService } from 'service/cardano/lucid/cardanoService.types';
import { CardanoKupoService } from 'service/http/kupo/kupoService.types';
import { ScriptReferenceService } from 'service/sequelize/scriptReferenceService/scriptReferenceService.types';

export interface PluginSdkService {
  sdkStartStop: AsyncStartStop;
  sdkTransaction: SdkTransaction;
  authenticationService: AuthenticationService;
  userService: UserService;
  scriptService: ScriptService;
  scriptReferenceService: ScriptReferenceService;
  cardanoService: CardanoService;
  cardanoKupoService: CardanoKupoService;
}
