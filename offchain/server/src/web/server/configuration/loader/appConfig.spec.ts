import loadAppConfigUnbound from './appConfig.unbound';
import nodeEnvConfigUnbound from './nodeEnv/nodeEnvConfig.unbound';
import databaseConfigUnbound from './database/databaseConfig.unbound';
import serverConfigUnbound from './server/serverConfig.unbound';
import ssoConfigUnbound from './sso/ssoConfig.unbound';
import loggerConfigUnbound from './logger/loggerConfig.unbound';
import lucidConfigUnbound from './lucid/lucidConfig.unbound';
import { AppConfig, AppConfigLoader } from './appConfig.types';
import { ENodeENV } from './nodeEnv/nodeEnvConfig.types';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { } from 'jest';

const env = {
  NODE_ENV: 'production',
  SSO_ISSUER: 'http://localhost:8101/auth/realms/demo',
  SSO_WELL_KNOWN: 'http://localhost:8101/auth/realms/demo/.well-known/openid-configuration',
  SSO_JWKS_URI: 'http://localhost:8101/auth/realms/demo/protocol/openid-connect/certs',
  SSO_TOKEN_ENDPOINT: 'http://localhost:8101/auth/realms/demo/protocol/openid-connect/token',
  SSO_END_SESSION_ENDPOINT: 'http://localhost:8101/auth/realms/demo/protocol/openid-connect/logout',
  SSO_HASH_ALG: 'RS256',
  SSO_CLIENT_ID: 'client_id',
  SSO_CLIENT_SECRET: 'client_secret',
  SSO_REDIRECT_URI: '= http://localhost:8080/callback',

  PROVIDER_TYPE: 'blockfrost',
  PROVIDER_BLOCKFROST_URL: 'https://cardano-preview.blockfrost.io/api/v0',
  PROVIDER_BLOCKFROST_PROJECT_ID: 'my_blockfrost_id'
};

describe('server configuration module', () => {
  describe(`'AppConfig'`, () => {
    const loadNodeEnvConfiguration: AppConfigLoader<Either<AppError, AppConfig>> = nodeEnvConfigUnbound(ENodeENV[env.NODE_ENV]);
    const loadDatabaseConfiguration: AppConfigLoader<AppConfig> = databaseConfigUnbound(env);
    const loadServerConfiguration: AppConfigLoader<AppConfig> = serverConfigUnbound(env);
    const loadSSOConfiguration: AppConfigLoader<Either<AppError, AppConfig>> = ssoConfigUnbound(env);
    const loadLoggerConfiguration: AppConfigLoader<AppConfig> = loggerConfigUnbound(env);
    const loadLucidConfiguration: AppConfigLoader<Either<AppError, AppConfig>> = lucidConfigUnbound(env);

    const loadAppConfig = loadAppConfigUnbound({
      loadNodeEnvConfiguration,
      loadDatabaseConfiguration,
      loadServerConfiguration,
      loadSSOConfiguration,
      loadLoggerConfiguration,
      loadLucidConfiguration
    });

    loadAppConfig()
      .do({
        right: (appConfig: AppConfig) => {
          it(`should have 6 own properties`, () => {
            expect(Object.keys(appConfig).length).toEqual(6);
          });
          it(`should have own property 'environment'`, () => {
            expect(appConfig).toHaveProperty('environment');
          });
          it(`should have own property 'server'`, () => {
            expect(appConfig).toHaveProperty('server');
          });
          it(`should have own property 'database'`, () => {
            expect(appConfig).toHaveProperty('database');
          });
          it(`should have own property 'sso'`, () => {
            expect(appConfig).toHaveProperty('sso');
          });
          it(`should have own property 'logger'`, () => {
            expect(appConfig).toHaveProperty('appLogger');
          });
          it(`should have own property 'lucid'`, () => {
            expect(appConfig).toHaveProperty('lucid');
          });
        },
        left: (error: AppError) => {
          throw new Error(`Left side has not been expected: ${error.message}`);
        }
      });
  });
});
