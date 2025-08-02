import filterBuilder from './filterBuilder';
import { ScritpCategory } from 'model/sequelize/model/script/script.types';
import { ScriptTypeExt } from 'model/cardano/cardano.types';

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Cardano', () => {
      describe('controller', () => {
        describe('script controller', () => {
          describe('get scripts', () => {
            describe('build filter', () => {
              it('should build empty filter', () => {
                expect(filterBuilder({})).toStrictEqual({});
              });

              it('should build Gift category filter', () => {
                expect(filterBuilder({ category: ScritpCategory.Gift })).toStrictEqual({ category: 'Gift' });
              });

              it('should build Native type filter', () => {
                expect(filterBuilder({ type: ScriptTypeExt.Native })).toStrictEqual({ type: 'Native' });
              });

              it('should build full filter', () => {
                expect(filterBuilder({ type: ScriptTypeExt.Native, category: ScritpCategory.Burn })).toStrictEqual({ type: 'Native', category: 'Burn' });
              });
            });
          });
        });
      });
    });
  });
});
