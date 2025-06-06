// import { Address, LucidEvolution, PrivateKey } from '@lucid-evolution/lucid';
// import { AppError } from 'common/error';
// import { Either } from 'tsmonad';
// import asyncLift from 'utils/monad/either/asyncLift/asyncLift';
// import doer from 'utils/monad/either/do/doer';


// export default (lucid: LucidEvolution) => (privateKey: PrivateKey): Promise<Either<AppError, Address>> =>
//   // makeWalletFromPrivateKey
//   Promise.resolve(Either.right(lucid))
//     .then(doer({
//       right: lucid => lucid.selectWallet.fromPrivateKey(privateKey)
//     }))
//     .then(asyncLift(lucid => lucid.wallet().address()));
