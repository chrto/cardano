import scriptService from './scriptService';
import { ScriptService } from './scriptService.types';

describe('Service', () => {
  describe('Sequelize', () => {
    describe('Script Service', () => {
      let service: ScriptService;

      beforeAll(() => {
        service = scriptService
          .apply(null, []);
      });

      it('Happy path', () => {
        expect(service).toBeInstanceOf(Object);
        expect(Object.keys(service)).toBeArrayOfSize(4);
        expect(service).toHaveProperty('getScriptById');
        expect(service).toHaveProperty('getScripts');
        expect(service).toHaveProperty('createScript');
        expect(service).toHaveProperty('deleteScript');
      });
    });
  });
});
