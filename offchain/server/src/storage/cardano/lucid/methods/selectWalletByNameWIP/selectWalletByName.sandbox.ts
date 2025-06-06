
// import selectWalletByName from './selectWalletByName';
// import { Lucid, LucidEvolution, PrivateKey, Provider } from '@lucid-evolution/lucid';
// import { Either } from 'tsmonad';
// import bind from 'utils/monad/either/bind/bind';
// import lucidConfig from 'web/server/configuration/loader/lucid/lucidConfig';
// import lift from 'utils/monad/either/lift/lift';
// import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
// import { ILucidConfig } from 'web/server/configuration/loader/lucid/lucidConfig.types';
// import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
// import doer from 'utils/monad/either/do/doer';
// import * as sniff from 'supersniff';
// import { getProvider } from '../lucid';
// import { AppError } from 'common/error';
// import { LucidError } from 'common/httpErrors';

// require('dotenv').config();

// const privateKey: PrivateKey = 'ed25519_sk1urvhyg45tulnsxukca607s3rqpy746hcrtmggekc3j50zqt8g2zsu55q0z'

// Promise.resolve(Either.right({}))
//   .then(bind(lucidConfig))
//   .then(lift((config: AppConfig): ILucidConfig => config.lucid))
//   .then(asyncBind((config: ILucidConfig): Promise<Either<AppError, LucidEvolution>> =>
//     getProvider(config)
//       .caseOf({
//         just: (provider: Provider) => Lucid(provider, config.network).then(Either.right<AppError, LucidEvolution>),
//         nothing: () => Promise.resolve(Either.left<AppError, LucidEvolution>(new LucidError('Provider has not been configured!')))
//       })
//   ))
//   .then(asyncBind((lucid: LucidEvolution) => selectWalletByName(lucid)(privateKey)))
//   .then(doer({
//     right: sniff,
//     left: e => sniff(e.message)
//   }))
//   .catch(sniff);
