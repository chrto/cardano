import context from './context';
import { AppRequest } from 'web/serverModules/types';
import { Context } from './context.types';

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Cardano', () => {
      describe('context', () => {
        let request: AppRequest = {
          implicits: {}
        } as AppRequest;
        let cardanoContext: Context;
        beforeAll(() => {
          cardanoContext = context.apply(null, [request]);
        });
        it(`Should create an object, which has 'Context' interface`, () => {
          expect(cardanoContext).toBeInstanceOf(Object);
          expect(cardanoContext).toStrictEqual({
            implicits: request.implicits
          });
        });
      });
    });
  });
});
