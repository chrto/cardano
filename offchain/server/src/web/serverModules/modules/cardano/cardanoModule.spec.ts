import cardanoModuleUnbound from './cardanoModule.unbound';
import { Router } from 'express';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { AuthenticationService } from 'service/http/authentication/types';
import { AsyncStartStop } from 'model/sequelize/modelFactory/modelFactory.types';
import { ModuleConfig, ModuleConfigFactory } from 'web/serverModules/types';
import { Context as CardanoContext } from './context/context.types';
import { AppConfig } from 'web/server/configuration/loader/appConfig.types';
import { IAppLogger } from 'logger/appLogger.types';

const AUTH_SERVICE: AuthenticationService = {} as AuthenticationService;
const SDK_START_STOP: AsyncStartStop = {} as AsyncStartStop;
const SERVICE: PluginSdkService = { authenticationService: AUTH_SERVICE, sdkStartStop: SDK_START_STOP } as PluginSdkService;
const APP_CONFIG: AppConfig = {
  appLogger: {
    fileLevel: 'debug'
  }
} as AppConfig;

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Cardano', () => {
      let logger: IAppLogger = {} as IAppLogger;

      let moduleDefinition: jest.Mock<ModuleConfigFactory<CardanoContext>, [PluginSdkService]>;
      let setModuleDefinition: jest.Mock<ModuleConfigFactory<CardanoContext>>;

      let moduleMiddlewares: jest.Mock<ModuleConfigFactory<CardanoContext>, [AppConfig]>;
      let setModuleMiddlewares: jest.Mock<ModuleConfigFactory<CardanoContext>>;

      let moduleParamHandler: jest.Mock<ModuleConfigFactory<CardanoContext>, [PluginSdkService]>;
      let setModuleParamHandler: jest.Mock<ModuleConfigFactory<CardanoContext>>;

      let registerErrorHandlerMiddleware: jest.Mock<ModuleConfig<CardanoContext>, [ModuleConfig<CardanoContext>]>;

      let registerRoutes: jest.Mock<ModuleConfig<CardanoContext>, [ModuleConfig<CardanoContext>]>;

      let result: Router;
      let moduleConfig: ModuleConfig<CardanoContext> = {
        router: {
          use: null
        } as Router,
        contextFactory: null,
        moduleDefinition: null
      };

      beforeAll(() => {
        jest.clearAllMocks();

        logger.error = jest.fn().mockReturnThis();

        setModuleDefinition = jest.fn().mockImplementation((moduleConfig: ModuleConfig<CardanoContext>): ModuleConfig<CardanoContext> => moduleConfig);
        moduleDefinition = jest.fn().mockReturnValue(setModuleDefinition);

        setModuleMiddlewares = jest.fn().mockImplementation((moduleConfig: ModuleConfig<CardanoContext>): ModuleConfig<CardanoContext> => moduleConfig);
        moduleMiddlewares = jest.fn().mockReturnValue(setModuleMiddlewares);

        setModuleParamHandler = jest.fn().mockImplementation((moduleConfig: ModuleConfig<CardanoContext>): ModuleConfig<CardanoContext> => moduleConfig);
        moduleParamHandler = jest.fn().mockReturnValue(setModuleParamHandler);

        registerErrorHandlerMiddleware = jest.fn().mockImplementation((moduleConfig: ModuleConfig<CardanoContext>): ModuleConfig<CardanoContext> => moduleConfig);

        registerRoutes = jest.fn().mockImplementation((moduleConfig: ModuleConfig<CardanoContext>): ModuleConfig<CardanoContext> => moduleConfig);

        result = cardanoModuleUnbound
          (logger, moduleDefinition, moduleMiddlewares, moduleParamHandler, registerErrorHandlerMiddleware, registerRoutes)
          (moduleConfig)
          (SERVICE, APP_CONFIG);
      });

      describe('Happy path', () => {
        it(`Should load module definition as first`, () => {
          expect(moduleDefinition).toHaveBeenCalledTimes(1);
          expect(moduleDefinition).toHaveBeenCalledWith(SERVICE);

          expect(moduleDefinition(SERVICE)).toBeFunction;
          expect(moduleDefinition(SERVICE)).toBe(setModuleDefinition);

          expect(setModuleDefinition).toHaveBeenCalledTimes(1);
          expect(setModuleDefinition).toHaveBeenCalledWith(moduleConfig);
        });

        it(`Should register required middlewares, after module definition has been loaded`, () => {
          expect(moduleMiddlewares).toHaveBeenCalledTimes(1);
          expect(moduleMiddlewares).toHaveBeenCalledWith(APP_CONFIG);

          expect(moduleMiddlewares(APP_CONFIG)).toBeFunction;
          expect(moduleMiddlewares(APP_CONFIG)).toBe(setModuleMiddlewares);

          expect(setModuleMiddlewares).toHaveBeenCalledTimes(1);
          expect(setModuleMiddlewares).toHaveBeenCalledWith(moduleConfig);

          expect(moduleMiddlewares).toHaveBeenCalledAfter(moduleDefinition);
        });

        it(`Should register required parameter handlers, after module middlewares has been registered`, () => {
          expect(moduleParamHandler).toHaveBeenCalledTimes(1);
          expect(moduleParamHandler).toHaveBeenCalledWith(SERVICE);

          expect(moduleParamHandler(SERVICE)).toBeFunction;
          expect(moduleParamHandler(SERVICE)).toBe(setModuleParamHandler);

          expect(setModuleParamHandler).toHaveBeenCalledTimes(1);
          expect(setModuleParamHandler).toHaveBeenCalledWith(moduleConfig);

          expect(moduleParamHandler).toHaveBeenCalledAfter(moduleMiddlewares);
        });

        it(`Should register routes from module definition object, after parameter handler has been registered`, () => {
          expect(registerRoutes).toHaveBeenCalledTimes(1);
          expect(registerRoutes).toHaveBeenCalledWith(moduleConfig);

          expect(registerRoutes).toHaveBeenCalledAfter(moduleParamHandler);
        });

        it(`Should register error handler middleware, after routes has been registered`, () => {
          expect(registerErrorHandlerMiddleware).toHaveBeenCalledTimes(1);
          expect(registerErrorHandlerMiddleware).toHaveBeenCalledWith(moduleConfig);

          expect(registerErrorHandlerMiddleware).toHaveBeenCalledAfter(registerRoutes);
        });

        it(`Should return router instance, after all required handlers have been registered`, () => {
          expect(result).toBe(moduleConfig.router);
        });
      });

      describe('Error path', () => {
        const ERROR_MSG: string = 'Route has not been registered!';
        let result;

        beforeAll(() => {
          jest.clearAllMocks();

          moduleMiddlewares = jest.fn().mockReturnValue((_moduleConfig: ModuleConfig<CardanoContext>): ModuleConfig<CardanoContext> => {
            throw new Error(ERROR_MSG);
          });

          try {
            result = cardanoModuleUnbound
              (logger, moduleDefinition, moduleMiddlewares, moduleParamHandler, registerErrorHandlerMiddleware, registerRoutes)
              (moduleConfig)
              (SERVICE, APP_CONFIG);
          } catch (e) {
            result = e;
          }
        });

        it('Should interrupt execution, after error has been received', () => {
          expect(moduleDefinition).toHaveBeenCalledTimes(1);
          expect(moduleMiddlewares).toHaveBeenCalledTimes(1);
          expect(registerRoutes).toHaveBeenCalledTimes(0);
        });

        it(`Should log exact error message`, () => {
          expect(logger.error).toHaveBeenCalledTimes(1);
          expect(logger.error).toHaveBeenCalledWith('Error in Cardano module loader..');
        });

        it(`Should throw exact error, if any function in chain has been thrown an error.`, () => {
          expect(result).toBeInstanceOf(Error);
          expect(result.message).toBe(ERROR_MSG);
        });
      });
    });
  });
});
