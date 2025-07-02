import cardanoNodeConfigUnbound from './cardanoNodeConfig.unbound';
import { AppConfig, AppConfigLoader } from '../appConfig.types';
import { ICardanoNodeConfig } from './cardanoNodeConfig.types';

const KUPO_URL = 'http://localhost:1442';
const OGMIOS_URL = 'http://localhost:1337';

describe('server configuration module', () => {
  describe(`'Cardano Local Node'`, () => {
    describe('Happy Path', () => {
      it(`Should use values from environment and set 'cardanoNode' configuration, if exists.`, () => {
        const env = {
          PROVIDER_NODE_KUPO_URL: KUPO_URL,
          PROVIDER_NODE_OGMIOS_URL: OGMIOS_URL
        };

        const expected: ICardanoNodeConfig = {
          kupoUrl: KUPO_URL,
          ogmiosUrl: OGMIOS_URL
        };

        const lucidConfig: AppConfigLoader<AppConfig> = cardanoNodeConfigUnbound.apply(null, [env]);
        const appConfig: AppConfig = lucidConfig();

        expect(appConfig)
          .toHaveProperty('cardanoNode', expected);
      });

      it(`Should NOT use values from environment and NOT set 'cardanoNode' configuration, if does not exists.`, () => {
        const env = {
        };

        const lucidConfig: AppConfigLoader<AppConfig> = cardanoNodeConfigUnbound.apply(null, [env]);
        const appConfig: AppConfig = lucidConfig();

        expect(appConfig).not.toHaveProperty('cardanoNode');
      });
    });
  });
});
