import { ENodeENV } from './nodeEnvConfig.types';
import nodeEnvConfigUnbound from './nodeEnvConfig.unbound';

export default nodeEnvConfigUnbound(ENodeENV[process.env.NODE_ENV]);
