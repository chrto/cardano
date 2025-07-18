describe('Web Server', () => {
  describe('Modules', () => {
    describe('Global', () => {
      it.todo('Logger init method');
    });
  });
});

// import globalModuleUnbound from './globalModule.unbound';
// import { Router } from 'express';
// import { Either } from 'tsmonad';
// import { Logger } from 'winston';
// import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
// import { AuthenticationService } from 'service/http/authentication/types';
// import { AsyncStartStop } from 'model/sequelize/modelFactory/modelFactory.types';
// import { ModuleConfig, ModuleConfigFactory } from 'web/serverModules/types';
// import { Context as GlobalContext } from './context/context.types';
// import { AppConfig } from 'web/server/configuration/loader/appConfig.types';

// const AUTH_SERVICE: AuthenticationService = {} as AuthenticationService;
// const SDK_START_STOP: AsyncStartStop = {} as AsyncStartStop;
// const SERVICE: PluginSdkService = { authenticationService: AUTH_SERVICE, sdkStartStop: SDK_START_STOP } as PluginSdkService;
// const APP_CONFIG: AppConfig = {
//   appLogger: {
//     fileLevel: 'debug'
//   }
// } as AppConfig;

// describe('Web Server', () => {
//   describe('Modules', () => {
//     describe('Global', () => {
//       let logger: Logger = {} as Logger;
//       let moduleDefinition: jest.Mock<ModuleConfigFactory<GlobalContext>, [PluginSdkService]>;
//       let setModuleDefinition: jest.Mock<ModuleConfigFactory<GlobalContext>, [PluginSdkService]>;
//       let configFactory: jest.Mock<ModuleConfigFactory<GlobalContext>>;
//       let moduleMiddlewares: jest.Mock<ModuleConfigFactory<GlobalContext>, [AppConfig]>;
//       let registerErrorHandlerMiddleware: jest.Mock<Either<any, ModuleConfig<GlobalContext>>, [ModuleConfig<GlobalContext>]>;
//       let registerRoutes: jest.Mock<ModuleConfig<GlobalContext>, [ModuleConfig<GlobalContext>]>;
//       let result: Router;
//       let moduleConfig: ModuleConfig<GlobalContext> = {
//         router: {
//           use: null
//         } as Router,
//         contextFactory: null,
//         moduleDefinition: null
//       };

//       beforeAll(() => {
//         jest.clearAllMocks();
//         logger.error = jest.fn().mockReturnThis();
//         setModuleDefinition = jest.fn().mockImplementation((moduleConfig: ModuleConfig<GlobalContext>): ModuleConfig<GlobalContext> => moduleConfig);
//         moduleDefinition = jest.fn().mockReturnValue(setModuleDefinition);
//         configFactory = jest.fn().mockImplementation((moduleConfig: ModuleConfig<GlobalContext>) => moduleConfig);
//         moduleMiddlewares = jest.fn().mockImplementation((_appConfig: AppConfig) => configFactory);
//         registerErrorHandlerMiddleware = jest.fn().mockImplementation((moduleConfig: ModuleConfig<GlobalContext>): ModuleConfig<GlobalContext> => moduleConfig);
//         registerRoutes = jest.fn().mockImplementation((moduleConfig: ModuleConfig<GlobalContext>): ModuleConfig<GlobalContext> => moduleConfig);

//         result = globalModuleUnbound
//           .apply(null, [logger, moduleDefinition, moduleMiddlewares, registerErrorHandlerMiddleware, registerRoutes])
//           .apply(null, [moduleConfig])
//           .apply(null, [SERVICE, APP_CONFIG]);
//       });

//       describe('Happy path', () => {
//         it(`Should load module definition as first`, () => {
//           expect(moduleDefinition).toHaveBeenCalledTimes(1);
//           expect(moduleDefinition).toHaveBeenCalledWith(SERVICE);
//           expect(moduleDefinition(SERVICE)).toBeFunction;
//           expect(moduleDefinition(SERVICE)).toBe(setModuleDefinition);
//           expect(setModuleDefinition).toHaveBeenCalledTimes(1);
//           expect(setModuleDefinition).toHaveBeenCalledWith(moduleConfig);
//         });

//         it(`Should register required middlewares, after module definition has been loaded`, () => {
//           expect(moduleMiddlewares).toHaveBeenCalledTimes(1);
//           expect(moduleMiddlewares).toHaveBeenCalledWith(APP_CONFIG);
//           expect(configFactory).toHaveBeenCalledWith(moduleConfig);
//           // expect(moduleMiddlewares).toHaveBeenCalledAfter(moduleDefinition);
//         });

//         it(`Should register routes from module definition object, after middlewares has been registered`, () => {
//           expect(registerRoutes).toHaveBeenCalledTimes(1);
//           expect(registerRoutes).toHaveBeenCalledWith(moduleConfig);
//           // expect(registerRoutes).toHaveBeenCalledAfter(moduleMiddlewares);
//         });

//         it(`Should register error handler middleware, after routes has been registered`, () => {
//           expect(registerErrorHandlerMiddleware).toHaveBeenCalledTimes(1);
//           expect(registerErrorHandlerMiddleware).toHaveBeenCalledWith(moduleConfig);
//           // expect(registerErrorHandlerMiddleware).toHaveBeenCalledAfter(registerRoutes);
//         });

//         it(`Should return router instance, after all required handlers have been registered`, () => {
//           expect(result).toBe(moduleConfig.router);
//         });
//       });

//       describe('Error path', () => {
//         const ERROR_MSG: string = 'Route has not been registered!';
//         let result: Error;

//         beforeAll(() => {
//           jest.clearAllMocks();

//           moduleMiddlewares = jest.fn().mockImplementation((_appConfig: AppConfig) => configFactory);
//           configFactory = jest.fn().mockImplementation((_moduleConfig: ModuleConfig<GlobalContext>): ModuleConfig<GlobalContext> => {
//             throw new Error(ERROR_MSG);
//           });
//           try {
//             globalModuleUnbound
//               .apply(null, [logger, moduleDefinition, moduleMiddlewares, registerErrorHandlerMiddleware, registerRoutes])
//               .apply(null, [moduleConfig])
//               .apply(null, [SERVICE, APP_CONFIG]);
//           } catch (e) {
//             result = e;
//           }
//         });

//         it('Should interrupt execution, after error has been received', () => {
//           expect(moduleDefinition).toHaveBeenCalledTimes(1);
//           expect(moduleMiddlewares).toHaveBeenCalledTimes(1);
//           expect(registerRoutes).toHaveBeenCalledTimes(0);
//         });

//         it(`Should log exact error message`, () => {
//           expect(logger.error).toHaveBeenCalledTimes(1);
//           expect(logger.error).toHaveBeenCalledWith('Error in global module loader..');
//         });

//         it(`Should throw exact error, if any function in chain has been thrown an error.`, () => {
//           expect(result).toBeInstanceOf(Error);
//           expect(result.message).toBe(ERROR_MSG);
//         });
//       });
//     });
//   });
// });
