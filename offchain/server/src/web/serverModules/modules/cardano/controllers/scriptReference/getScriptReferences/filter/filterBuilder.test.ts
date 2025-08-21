import filterBuilder from './filterBuilder';

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Cardano', () => {
      describe('controller', () => {
        describe('script reference controller', () => {
          describe('get script references', () => {
            describe('build filter', () => {
              it('should build empty filter', () => {
                expect(filterBuilder({})).toStrictEqual({});
              });

              it('should build scriptId filter', () => {
                expect(filterBuilder({ scriptId: 'f0962fc9-882d-416d-bc08-fed1d5aa3a36' })).toStrictEqual({ scriptId: 'f0962fc9-882d-416d-bc08-fed1d5aa3a36' });
              });

              it('should build address filter', () => {
                expect(filterBuilder({ address: 'addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla' })).toStrictEqual({ address: 'addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla' });
              });

              it('should build unspend filter', () => {
                expect(filterBuilder({ unspend: true })).toStrictEqual({ unspend: true });
              });
            });
          });
        });
      });
    });
  });
});
