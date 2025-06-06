// import { DEFAULT_DB_ALLOW_LOGGING, DEFAULT_DB_ALLOW_SYNC, DEFAULT_DB_DIALECT, DEFAULT_DB_URL } from 'src/defaults';
// import { AppConfig, AppConfigLoader } from '../appConfig.types';
// import { EDatabaseDialect, IDatabaseConfig } from './databaseConfig.types';
import lucidConfigUnbound from './lucidConfig.unbound';
import { AppConfig, AppConfigLoader } from '../appConfig.types';
import { ILucidConfig, ProviderType } from './lucidConfig.types';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { DEFAULT_PROVIDER_NETWORK } from 'src/defaults';
import InvalidConfiguraton from '../../error/configuration/error';
import { Network } from 'model/cardano/cardano.types';

describe('server configuration module', () => {
  describe(`'lucid'`, () => {
    describe('Happy Path', () => {
      it(`Should use values from environment, if exists (blockfrost).`, () => {
        const env = {
          PROVIDER_TYPE: 'blockfrost',
          PROVIDER_BLOCKFROST_URL: 'https://cardano-preview.blockfrost.io/api/v0',
          PROVIDER_BLOCKFROST_PROJECT_ID: 'my_proj_id',
          PROVIDER_NETWORK: 'Mainnet'
        };

        const expected: ILucidConfig = {
          providerType: ProviderType.blockfrost,
          provider: {
            blockfrost: {
              url: env.PROVIDER_BLOCKFROST_URL,
              projectId: env.PROVIDER_BLOCKFROST_PROJECT_ID
            }
          },
          network: Network.Mainnet
        };

        const lucidConfig: AppConfigLoader<Either<AppError, AppConfig>> = lucidConfigUnbound.apply(null, [env]);

        lucidConfig()
          .do({
            right: (appConfig: AppConfig) =>
              expect(appConfig)
                .toHaveProperty('lucid', expected),
            left: (error: AppError) => ('Left side was not expected.' + '\n' + error.code + '\n' + error.message)
          });
      });

      it(`Should use values from environment, if exists (local node).`, () => {
        const env = {
          PROVIDER_TYPE: 'node',
          PROVIDER_NODE_OGMIOS_URL: 'http://localhost:1337',
          PROVIDER_NODE_KUPO_URL: 'http://localhost:1442',
          PROVIDER_NETWORK: 'Mainnet'
        };

        const expected: ILucidConfig = {
          providerType: ProviderType.node,
          provider: {
            node: {
              ogmiosUrl: env.PROVIDER_NODE_OGMIOS_URL,
              kupoUrl: env.PROVIDER_NODE_KUPO_URL
            }
          },
          network: Network.Mainnet
        };

        const lucidConfig: AppConfigLoader<Either<AppError, AppConfig>> = lucidConfigUnbound.apply(null, [env]);

        lucidConfig()
          .do({
            right: (appConfig: AppConfig) =>
              expect(appConfig)
                .toHaveProperty('lucid', expected),
            left: (error: AppError) => ('Left side was not expected.' + '\n' + error.code + '\n' + error.message)
          });
      });

      it(`Should use values from 'defaults.ts', if does not find in environment.`, () => {
        const env = {
          PROVIDER_TYPE: 'blockfrost',
          PROVIDER_BLOCKFROST_URL: 'https://cardano-preview.blockfrost.io/api/v0',
          PROVIDER_BLOCKFROST_PROJECT_ID: 'my_proj_id',
        };

        const expected: ILucidConfig = {
          providerType: ProviderType.blockfrost,
          provider: {
            blockfrost: {
              url: env.PROVIDER_BLOCKFROST_URL,
              projectId: env.PROVIDER_BLOCKFROST_PROJECT_ID
            },
          },
          network: DEFAULT_PROVIDER_NETWORK
        };
        const lucidConfig: AppConfigLoader<Either<AppError, AppConfig>> = lucidConfigUnbound.apply(null, [env]);

        lucidConfig()
          .do({
            right: (appConfig: AppConfig) =>
              expect(appConfig)
                .toHaveProperty('lucid', expected),
            left: (error: AppError) => fail('Left side was not expected.' + '\n' + error.code + '\n' + error.message)
          });
      });
    });

    describe('Error Path', () => {
      it(`Should fail, if configuration missing.`, () => {
        const env = {

        };

        const expected: InvalidConfiguraton = new InvalidConfiguraton('Lucid configuration was not loaded.');

        const lucidConfig: AppConfigLoader<Either<AppError, AppConfig>> = lucidConfigUnbound.apply(null, [env]);

        lucidConfig()
          .do({
            right: (_: AppConfig) => fail('Right side has not been expected!'),
            left: (error: AppError) => {
              expect(error).toBeInstanceOf(AppError);
              expect(error).toBeInstanceOf(InvalidConfiguraton);
              expect(error.message).toEqual(expected.message);
            }
          });
      });

      it(`Should fail, if PROVIDER_TYPE missing.`, () => {
        const env = {
          PROVIDER_BLOCKFROST_URL: 'https://cardano-preview.blockfrost.io/api/v0',
          PROVIDER_BLOCKFROST_PROJECT_ID: 'my_proj_id',
        };

        const expected: InvalidConfiguraton = new InvalidConfiguraton('Lucid configuration was not loaded.');

        const lucidConfig: AppConfigLoader<Either<AppError, AppConfig>> = lucidConfigUnbound.apply(null, [env]);

        lucidConfig()
          .do({
            right: (_: AppConfig) => fail('Right side has not been expected!'),
            left: (error: AppError) => {
              expect(error).toBeInstanceOf(AppError);
              expect(error).toBeInstanceOf(InvalidConfiguraton);
              expect(error.message).toEqual(expected.message);
            }
          });
      });

      it(`Should fail, if provider missing.`, () => {
        const env = {
          PROVIDER_TYPE: 'blockfrost',
        };

        const expected: InvalidConfiguraton = new InvalidConfiguraton('Lucid configuration was not loaded.');

        const lucidConfig: AppConfigLoader<Either<AppError, AppConfig>> = lucidConfigUnbound.apply(null, [env]);

        lucidConfig()
          .do({
            right: (_: AppConfig) => fail('Right side has not been expected!'),
            left: (error: AppError) => {
              expect(error).toBeInstanceOf(AppError);
              expect(error).toBeInstanceOf(InvalidConfiguraton);
              expect(error.message).toEqual(expected.message);
            }
          });
      });
    });
  });
});
