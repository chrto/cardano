import { isMissing } from 'utils/validation';
import { AppConfig } from '../appConfig.types';

const isENVConfigured = (env: NodeJS.ProcessEnv): boolean =>
  !isMissing(env.PROVIDER_NODE_OGMIOS_URL) && !isMissing(env.PROVIDER_NODE_KUPO_URL);

export default (env: NodeJS.ProcessEnv) =>
  (appConfig: AppConfig = {} as AppConfig): AppConfig =>
    isENVConfigured(env)
      ? {
        ...appConfig,
        cardanoNode: {
          kupoUrl: env.PROVIDER_NODE_KUPO_URL,
          ogmiosUrl: env.PROVIDER_NODE_OGMIOS_URL
        }
      }
      : appConfig;
