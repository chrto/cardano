import { isMissing } from 'utils/validation';
import { Either } from 'tsmonad';
import { AppConfig } from '../appConfig.types';
import { AppError } from 'common/error';
import InvalidConfiguraton from '../../error/configuration/error';
import { DEFAULT_PROVIDER_NETWORK } from 'src/defaults';
import { Network } from 'model/cardano/cardano.types';
import { ProviderType } from './lucidConfig.types';

const isENVConfigured = (env: NodeJS.ProcessEnv): boolean =>
(
  (env.PROVIDER_TYPE === 'blockfrost' && !isMissing(env.PROVIDER_BLOCKFROST_URL) && !isMissing(env.PROVIDER_BLOCKFROST_PROJECT_ID)) ||
  (env.PROVIDER_TYPE === 'node' && !isMissing(env.PROVIDER_NODE_OGMIOS_URL) && !isMissing(env.PROVIDER_NODE_KUPO_URL))
);

export const getNetwork = (network: string): Network => {
  switch (network) {
    case 'Mainnet': return Network.Mainnet;
    case 'Preview': return Network.Preview;
    case 'Preprod': return Network.Preprod;
    case 'Custom': return Network.Custom;
    default: return DEFAULT_PROVIDER_NETWORK;
  }
};

export const getProviderType = (providerType: string): ProviderType => {
  switch (providerType) {
    case 'node': return ProviderType.node;
    case 'blockfrost': return ProviderType.blockfrost;
    default: return null;
  }
};

// exoprt const getProvider env =

export default (env: NodeJS.ProcessEnv) =>
  (appConfig: AppConfig = {} as AppConfig): Either<AppError, AppConfig> =>
    isENVConfigured(env)
      ? Either.right<AppError, ProviderType>(getProviderType(env.PROVIDER_TYPE))
        .lift((providerType) => ({
          ...appConfig,
          lucid: {
            providerType,
            provider: {
              ...providerType === ProviderType.blockfrost && {
                blockfrost: {
                  url: env.PROVIDER_BLOCKFROST_URL,
                  projectId: env.PROVIDER_BLOCKFROST_PROJECT_ID
                }
              },
              ...providerType === ProviderType.node && {
                node: {
                  kupoUrl: env.PROVIDER_NODE_KUPO_URL,
                  ogmiosUrl: env.PROVIDER_NODE_OGMIOS_URL
                }
              }
            },
            network: getNetwork(env.PROVIDER_NETWORK)
          }
        }))
      : Either.left<AppError, AppConfig>(new InvalidConfiguraton('Lucid configuration was not loaded.'));
