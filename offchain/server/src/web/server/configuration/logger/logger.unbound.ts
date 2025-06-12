import { AppConfig } from '../loader/appConfig.types';
import { ENodeENV } from '../loader/nodeEnv/nodeEnvConfig.types';
import { ProviderType } from '../loader/lucid/lucidConfig.types';
import { IAppLogger } from 'logger/appLogger.types';

export default (logger: IAppLogger) =>
  (appConfig: AppConfig): void => {
    appConfig.environment === ENodeENV.development &&
      logger
        .debug('Environment setup:')
        .debug(`'NODE_ENV'                   ='${appConfig.environment}'`)
        .debug('')

        .debug('Server setup:')
        .debug(`'API port'                   ='${appConfig.server.apiPort}'`)
        .debug(`'shutdown port'              ='${appConfig.server.shutdownPort}'`)
        .debug(`'shutdown timeout'           ='${appConfig.server.shutdownTimeout}'`)
        .debug(`'startup delay'              ='${appConfig.server.startupDelay}'`)
        .debug(`'retry count'                ='${appConfig.server.retryCount}'`)
        .debug('')

        .debug('SSO setup:')
        .debug(`'issuer'                     ='${appConfig.sso.ssoIssuer}'`)
        .debug(`'JWKS uri'                   ='${appConfig.sso.ssoJwksUri}'`)
        .debug(`'well-known endpoint         ='${appConfig.sso.ssoWellKnown}'`)
        .debug(`'token endpoint'             ='${appConfig.sso.ssoTokenEndpoint}'`)
        .debug(`'end session endpoint'       ='${appConfig.sso.ssoEndSessionEndpoint}'`)
        .debug(`'client ID'                  ='${appConfig.sso.ssoClientId}'`)
        .debug(`'client secret'              ='${appConfig.sso.ssoClientSecret}'`)
        .debug(`'hash alg.'                  ='${appConfig.sso.ssoHashAlg}'`)
        .debug(`'redirect uri'               ='${appConfig.sso.ssoRedirectUri}'`)
        .debug('')

        .debug('Database setup:')
        .debug(`'DB URL'                     ='${appConfig.database.url}'`)
        .debug(`'DB dialect'                 ='${appConfig.database.dialect}'`)
        .debug(`'DB allow sync'              ='${appConfig.database.allowSync}'`)
        .debug(`'DB allow logging'           ='${appConfig.database.allowLogging}'`)
        .debug('')

        .debug('Cardano Provider setup:')
        .debug(`'Provider'                   ='${appConfig.lucid.providerType}'`)
        .debug(`'Network'                    ='${appConfig.lucid.network}'`)
        .debug(`'BlockFrost URL'             ='${appConfig.lucid.provider.blockfrost?.url}'`, appConfig.lucid.providerType !== ProviderType.blockfrost) // || !appConfig.lucid.provider.blockfrost.url)
        .debug(`'BlockFrost api key'         ='${appConfig.lucid.provider.blockfrost?.projectId}'`, appConfig.lucid.providerType !== ProviderType.blockfrost)
        .debug(`'Kupo URL'                   ='${appConfig.lucid.provider.node?.kupoUrl}'`, appConfig.lucid.providerType !== ProviderType.node)
        .debug(`'Ogmios URL'                 ='${appConfig.lucid.provider.node?.ogmiosUrl}'`, appConfig.lucid.providerType !== ProviderType.node)
        .debug('')

        .debug('Logger setup:')
        .debug(`'Log label'                  ='${appConfig.appLogger.label}'`)
        .debug('File log:')
        .debug(`'Directory'                  ='${appConfig.appLogger.dir}'`)
        .debug(`'Level'                      ='${appConfig.appLogger.fileLevel}'`)
        .debug(`'Log file name'              ='${appConfig.appLogger.fileNameInfo}'`)
        .debug(`'Error log file name'        ='${appConfig.appLogger.fileNameError}'`)
        .debug(`'Exceptions log file name'   ='${appConfig.appLogger.fileNameExceptions}'`)
        .debug(`'Rotate log pattern'         ='${appConfig.appLogger.fileDatePattern}'`)
        .debug(`'Archive log files'          ='${appConfig.appLogger.fileZipArchive}'`)
        .debug(`'Log file max size'          ='${appConfig.appLogger.fileMaxSize}'`)
        .debug(`'Log file max'               ='${appConfig.appLogger.fileMaxFiles}'`)
        .debug('')

        .debug('Console log:')
        .debug(`'Enable'                     ='${appConfig.appLogger.consoleEnable}'`)
        .debug(`'Level'                      ='${appConfig.appLogger.consoleLevel}'`, !appConfig.appLogger.consoleEnable)
        .debug('')

        .debug('Splunk log:')
        .debug(`'Enable'                     ='${appConfig.appLogger.splunkEnable}'`)
        .debug(`'Level'                      ='${appConfig.appLogger.splunkLevel}'`, !appConfig.appLogger.splunkEnable || !appConfig.appLogger.fileLevel)
        .debug(`'Host'                       ='${appConfig.appLogger.splunkHost}'`, !appConfig.appLogger.splunkEnable || !appConfig.appLogger.splunkHost)
        .debug(`'Port'                       ='${appConfig.appLogger.splunkPort}'`, !appConfig.appLogger.splunkEnable || !appConfig.appLogger.splunkPort)
        .debug(`'Path'                       ='${appConfig.appLogger.splunkPath}'`, !appConfig.appLogger.splunkEnable || !appConfig.appLogger.splunkPath)
        .debug(`'Token'                      ='${appConfig.appLogger.splunkToken}'`, !appConfig.appLogger.splunkEnable || !appConfig.appLogger.splunkToken)
        .debug(`'Index'                      ='${appConfig.appLogger.splunkIndex}'`, !appConfig.appLogger.splunkEnable || !appConfig.appLogger.splunkIndex)
        .debug(`'Source'                     ='${appConfig.appLogger.splunkSource}'`, !appConfig.appLogger.splunkEnable || !appConfig.appLogger.splunkSource)
        .debug(`'Source type'                ='${appConfig.appLogger.splunkSourceType}'`, !appConfig.appLogger.splunkEnable || !appConfig.appLogger.splunkSourceType)
        .debug('');
  };
