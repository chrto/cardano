import scriptReferenceFactoryUnbound from './scriptReferenceFactory.unbound';
import { AppError } from 'common/error';
import { Factory } from 'common/types';
import { Either } from 'tsmonad';
import { ScriptReferenceItems, ScriptReferenceRequired } from '../scriptReference.types';

describe('sequelize model', () => {
  describe('script reference', () => {
    describe('factory', () => {
      const UUID: string = 'f923b2c9-ffcf-4a0a-bdc9-a4a4ae2a687e';
      const uuidGenrator: () => string = () => UUID;
      const scriptFactory: Factory<ScriptReferenceRequired, Either<AppError, ScriptReferenceItems>> = scriptReferenceFactoryUnbound.apply(null, [uuidGenrator]);
      const scriptReferenceRequired: ScriptReferenceRequired = {
        scriptId: 'f0962fc9-882d-416d-bc08-fed1d5aa3a36',
        address: 'addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla',
        txId: '82e75104c2ffcab389fae6a9c87ebbe99e83cd7826d02534e77783b12c62e467',
        txIndex: 0,
        unspend: true
      };

      it('Should return script reference items in right side', () => {
        const expected: ScriptReferenceItems = {
          id: UUID,
          scriptId: 'f0962fc9-882d-416d-bc08-fed1d5aa3a36',
          address: 'addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla',
          txId: '82e75104c2ffcab389fae6a9c87ebbe99e83cd7826d02534e77783b12c62e467',
          txIndex: 0,
          unspend: true
        };

        scriptFactory({ ...scriptReferenceRequired })
          .do({
            right: (items: ScriptReferenceItems) =>
              expect(items).toStrictEqual(expected),
            left: (error: AppError) =>
              fail(`Left side has not been expected: ${error.message}`)
          });
      });
    });
  });
});
