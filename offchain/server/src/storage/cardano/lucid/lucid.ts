import { Lucid, Blockfrost, Provider, getAddressDetails as getAddressDetailsLucid, Kupmios, LucidEvolution } from '@lucid-evolution/lucid';
import buildTtransactionFromAddress from './methods/buildTtransactionFromAddress/buildTtransactionFromAddress';
import getAddressUTxOs from './methods/getAddressUTxOs/getAddressUTxOs';
import getValidatorAddress from './methods/getValidatorAddress/getValidatorAddress';
import getAddressDetail from './methods/getAddressDetail/getAddressDetail';
import buildTtransactionFromUTxOs from './methods/buildTtransactionFromUTxOs/buildTtransactionFromUTxOs';
import submitTtransaction from './methods/submitTtransaction/submitTtransaction';
import { CardanoStorage, LucidStorage } from './lucid.types';
import { ILucidConfig, ProviderType } from 'web/server/configuration/loader/lucid/lucidConfig.types';
import { Either, Maybe } from 'tsmonad';
import { LucidError } from 'common/httpErrors';
import { AppError } from 'common/error';
import lift from 'utils/monad/either/lift/lift';

const lucidProviderFactory = (lucidConfig: ILucidConfig): Maybe<Provider> =>
  lucidConfig.providerType === ProviderType.blockfrost
    ? Maybe.just(new Blockfrost(lucidConfig.provider.blockfrost.url, lucidConfig.provider.blockfrost.projectId))
    : lucidConfig.providerType === ProviderType.node
      ? Maybe.just(new Kupmios(lucidConfig.provider.node.kupoUrl, lucidConfig.provider.node.ogmiosUrl))
      : Maybe.nothing();

export const lucidFactory = (lucidConfig: ILucidConfig): Promise<Either<AppError, LucidStorage>> =>
  Lucid(
    lucidProviderFactory(lucidConfig)
      .caseOf({
        just: (provider: Provider) => provider,
        nothing: () => undefined
      }),
    lucidConfig.network
  )
    .then((lucidEvolution: LucidEvolution) => Either.right<AppError, LucidStorage>({ lucidEvolution }))
    .catch(e => Either.left<AppError, LucidStorage>(new LucidError(`Lucid has not been initialized: ${e.message}`)));

export default (lucidConfig: ILucidConfig): Promise<Either<AppError, CardanoStorage>> =>
  lucidFactory(lucidConfig)
    .then(lift((lucid: LucidStorage) => ({
      getAddressUTxOs: getAddressUTxOs(lucid.lucidEvolution),
      getValidatorAddress: getValidatorAddress(lucid.lucidEvolution),
      getAddressDetail: getAddressDetail(getAddressDetailsLucid),
      buildTransactionFromAddress: buildTtransactionFromAddress(lucid.lucidEvolution),
      buildTransactionFromUTxOs: buildTtransactionFromUTxOs(lucid.lucidEvolution),
      submitTransaction: submitTtransaction(lucid.lucidEvolution)
    })));
