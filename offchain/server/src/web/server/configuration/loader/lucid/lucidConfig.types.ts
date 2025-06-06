import { Network } from 'model/cardano/cardano.types';

export enum ProviderType {
  node = 'node',
  blockfrost = 'blockfrost'
}

export interface ILucidConfig {
  providerType: ProviderType;
  provider: {
    blockfrost?: {
      url: string;
      projectId: string;
    };
    node?: {
      kupoUrl: string;
      ogmiosUrl: string;
    };

  };
  network: Network;
}
