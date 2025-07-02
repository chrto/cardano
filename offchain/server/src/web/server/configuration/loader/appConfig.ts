import appConfigUnbound from './appConfig.unbound';
import loadNodeEnvConfiguration from './nodeEnv/nodeEnvConfig';
import loadServerConfiguration from './server/serverConfig';
import loadDatabaseConfiguration from './database/databaseConfig';
import loadSSOConfiguration from './sso/ssoConfig';
import loadLoggerConfiguration from './logger/loggerConfig';
import loadLucidConfiguration from './lucid/lucidConfig';
import loadCardanoNodeConfiguration from './cardanoNode/cardanoNodeConfig';

export default appConfigUnbound({
  loadNodeEnvConfiguration,
  loadServerConfiguration,
  loadDatabaseConfiguration,
  loadSSOConfiguration,
  loadLoggerConfiguration,
  loadLucidConfiguration,
  loadCardanoNodeConfiguration
});
