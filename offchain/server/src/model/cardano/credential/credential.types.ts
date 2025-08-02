import { CredentialType, KeyHash, ScriptHash } from '../cardano.types';

export type Credential = {
  type: CredentialType;
  hash: KeyHash | ScriptHash;
};
