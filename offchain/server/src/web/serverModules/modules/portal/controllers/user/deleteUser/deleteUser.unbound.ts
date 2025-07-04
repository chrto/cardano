import bind from 'utils/monad/either/bind/bind';
import lift from 'utils/monad/either/lift/lift';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import makeSure from 'utils/monad/either/makeSure/makeSure';
import { User as PortalUser } from 'model/sequelize/model/user/user';
import { Either } from 'tsmonad';
import { AppError } from 'common/error';
import { DeletedUser } from './deleteUser.types';
import { AppRequest } from 'web/serverModules/types';
import { RequestImplicits } from '../../../paramHandlers/paramHandlers.types';
import { Context as PortalContext } from './../../../context/context.types';
import { NotAuthorized } from 'common/httpErrors';
import { Fcn } from 'common/types';
import { UserService } from 'service/sequelize/userService/userService.types';
import { Response } from 'express';

export default (isDifferentUser: Fcn<[PortalUser, PortalUser], boolean>) =>
  ({ deleteUser }: UserService) =>
    async (ctx: PortalContext, _req: AppRequest<PortalUser, RequestImplicits>, _res: Response): Promise<Either<AppError, DeletedUser>> =>
      Promise.resolve(Either.right(ctx.implicits.user))
        .then(bind(makeSure(isDifferentUser.bind(null, ctx.loggedInUser), new NotAuthorized('User cannot delete himself'))))
        .then(asyncBind(deleteUser()))
        .then(lift((_: number): DeletedUser => ({ removedUserId: ctx.implicits.user.id })));
