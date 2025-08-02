import { AppError } from 'common/error';
import scriptFactoryUnbound from './scriptFactory.unbound';
import { Factory } from 'common/types';
import { Either } from 'tsmonad';
import { ScriptRequired, ScriptItems, ScritpCategory } from '../script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';

describe('sequelize model', () => {
  describe('user', () => {
    describe('factory', () => {
      const UUID: string = 'f923b2c9-ffcf-4a0a-bdc9-a4a4ae2a687e';
      const uuidGenrator: () => string = () => UUID;
      const scriptFactory: Factory<ScriptRequired, Either<AppError, ScriptItems>> = scriptFactoryUnbound.apply(null, [uuidGenrator]);
      const scriptRequired: ScriptRequired = {
        type: PlutusVersion.PlutusV2,
        script: '49480100002221200101',
        category: ScritpCategory.Gift,
        title: 'PPP',
        description: '...'
      };

      it('Should return script items in right side', () => {
        const expected: ScriptItems = {
          id: UUID,
          type: PlutusVersion.PlutusV2,
          script: '49480100002221200101',
          category: ScritpCategory.Gift,
          title: 'PPP',
          description: '...'
        };

        scriptFactory({ ...scriptRequired })
          .do({
            right: (items: ScriptItems) =>
              expect(items).toStrictEqual(expected),
            left: (error: AppError) =>
              fail(`Left side has not been expected: ${error.message}`)
          });
      });
    });
  });
});
