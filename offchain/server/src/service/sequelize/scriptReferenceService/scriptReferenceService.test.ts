import scriptReferenceService from './scriptReferenceService';
import { ScriptReferenceService } from './scriptReferenceService.types';

describe('Service', () => {
  describe('Sequelize', () => {
    describe('ScriptReferenceService Service', () => {
      let service: ScriptReferenceService;

      beforeAll(() => {
        service = scriptReferenceService
          .apply(null, []);
      });

      it('Happy path', () => {
        expect(service).toBeInstanceOf(Object);
        expect(Object.keys(service)).toBeArrayOfSize(3);
        expect(service).toHaveProperty('getScriptReferenceById');
        expect(service).toHaveProperty('createScriptReference');
        expect(service).toHaveProperty('getScriptReferences');
      });
    });
  });
});
