import { ScriptType } from '../cardano.types';

export type Validator = MintingPolicy | SpendingValidator | CertificateValidator | WithdrawalValidator | VoteValidator | ProposeValidator;
export type MintingPolicy = Script;
export type SpendingValidator = Script;
export type CertificateValidator = Script;
export type WithdrawalValidator = Script;
export type VoteValidator = Script;
export type ProposeValidator = Script;

export interface Script {
  type: ScriptType;
  script: string;
}
