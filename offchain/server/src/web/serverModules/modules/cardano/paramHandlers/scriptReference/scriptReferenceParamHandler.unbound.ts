import makeSure from 'utils/monad/either/makeSure/makeSure';
import caseOf from 'utils/monad/either/caseOf/caseOf';
import bind from 'utils/monad/either/bind/bind';
import asyncBind from 'utils/monad/either/asyncBind/asyncBind';
import { Either } from 'tsmonad';
import { NextFunction, Response } from 'express';
import { AppError } from 'common/error';
import { InvalidInput } from 'common/httpErrors';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { AppRequest } from 'web/serverModules/types';
import { RequestImplicits } from '../paramHandlers.types';
import { Fcn } from 'common/types';
import { ScriptReference as CardanoScriptReference } from 'model/sequelize/model/scriptReference/scriptReference';

export default (
  addEntityInToRequestImplicits: Fcn<[AppRequest<unknown, RequestImplicits>, Response, NextFunction, string], Fcn<[CardanoScriptReference], void>>,
  handleError: Fcn<[AppRequest<unknown, RequestImplicits>, Response, NextFunction, string], Fcn<[AppError], void>>,
  isUuid: Fcn<[string], boolean>
) =>
  ({ scriptReferenceService }: PluginSdkService) =>
    async (request: AppRequest<unknown, RequestImplicits>, response: Response, next: NextFunction, scriptReferenceId: string): Promise<void> =>
      Promise.resolve(Either.right<AppError, string>(scriptReferenceId))
        .then(bind(makeSure(isUuid, new InvalidInput(`scriptReferenceId ${scriptReferenceId} is not valid uuid`))))
        .then(asyncBind(scriptReferenceService.getScriptReferenceById()))
        .then(caseOf({
          right: addEntityInToRequestImplicits(request, response, next, 'scriptReference'),
          left: handleError(request, response, next, `ScriptReference with scriptReferenceId ${scriptReferenceId} not found`)
        }))
        .catch(next);
