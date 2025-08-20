import scriptReferenceController from './scriptReferenceController';
import { PluginSdkService } from 'service/serviceFactory/serviceFactory.types';
import { ScriptReferenceController } from './scriptReferenceController.types';

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Cardano', () => {
      describe('controller', () => {
        describe('script reference controller', () => {
          let controller: ScriptReferenceController;

          beforeAll(() => {
            controller = scriptReferenceController({ scriptReferenceService: {} } as PluginSdkService);
          });

          it('Happy path', () => {
            expect(controller).toBeObject();
            expect(Object.keys(controller)).toBeArrayOfSize(3);

            expect(controller).toHaveProperty('getScriptReferenceById');
            expect(controller).toHaveProperty('getScriptReferences');
            expect(controller).toHaveProperty('createScriptReference');
          });
        });
      });
    });
  });
});
