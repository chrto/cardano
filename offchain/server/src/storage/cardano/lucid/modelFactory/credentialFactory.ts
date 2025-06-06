import { Credential } from '@lucid-evolution/lucid';
import { Credential as CredentialModel } from 'model/cardano/credential/credential.types';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { fromCredentialType, toCredentialType } from './typesFactory';
import { CredentialType } from 'model/cardano/cardano.types';

export const toCredential = (credential: CredentialModel): Either<AppError, Credential> =>
  toCredentialType(credential.type)
    .lift((type: 'Key' | 'Script') => ({ type, hash: credential.hash }));

export default (credential: Credential): Either<AppError, CredentialModel> =>
  fromCredentialType(credential.type)
    .lift((type: CredentialType) => ({ type, hash: credential.hash }));
