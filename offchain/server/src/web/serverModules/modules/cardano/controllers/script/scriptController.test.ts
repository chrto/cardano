import scriptController from './scriptController';
import { ScriptController } from './scriptController.types';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Cardano', () => {
      describe('controller', () => {
        describe('script controller', () => {
          let controller: ScriptController;

          beforeAll(() => {
            controller = scriptController({ cardanoService: {}, scriptService: {} } as PluginSdkService);
          });

          it('Happy path', () => {
            expect(controller).toBeObject();
            expect(Object.keys(controller).length).toEqual(4);

            expect(controller).toHaveProperty('getScirptAddress');
            expect(controller).toHaveProperty('getScriptById');
            expect(controller).toHaveProperty('getScripts');
            expect(controller).toHaveProperty('createScript');
          });
        });
      });
    });
  });
});
