import bodyValidator from './bodyValidator';

import { AppError } from 'common/error';
import { InvalidInput } from 'common/httpErrors';
import { ScriptReferenceBody } from '../createScriptReference.types';

const BODY: ScriptReferenceBody = {
  scriptId: 'f0962fc9-882d-416d-bc08-fed1d5aa3a36',
  address: 'addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla',
  txId: '82e75104c2ffcab389fae6a9c87ebbe99e83cd7826d02534e77783b12c62e467',
  txIndex: 0
};

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Caredano', () => {
      describe('controller', () => {
        describe('script reference controller', () => {
          describe('create script reference', () => {
            describe('body validator', () => {
              it(`Should return Either with body object in right side`, () => {
                bodyValidator(BODY)
                  .do({
                    right: (body: ScriptReferenceBody) => {
                      expect(body).toBeObject;
                      expect(body).toStrictEqual(BODY);
                    },
                    left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
                  });
              });

              it(`Should have mandatory fields`, () => {
                const ERROR_MESSAGE: string = 'following required properties are missing in request: scriptId, address, txId, txIndex';
                bodyValidator({})
                  .do({
                    right: (): void => fail(`Right side has not been expected`),
                    left: (error: AppError) => {
                      expect(error).toBeInstanceOf(InvalidInput);
                      expect(error.message)
                        .toEqual(ERROR_MESSAGE);
                    }
                  });
              });

              it(`scriptId must be specify`, () => {
                const ERROR_MESSAGE: string = 'Validation failed: ["Missing mandatory property scriptId"]';
                bodyValidator({ ...BODY, scriptId: '' })
                  .do({
                    right: (): void => fail(`Right side has not been expected`),
                    left: (error: AppError) => {
                      expect(error)
                        .toBeInstanceOf(InvalidInput);
                      expect(error.message)
                        .toEqual(ERROR_MESSAGE);
                    }
                  });
              });

              it(`scriptId must be valid UUID`, () => {
                const ERROR_MESSAGE: string = 'Validation failed: ["scriptId is not valid UUID"]';
                bodyValidator({ ...BODY, scriptId: 'aaaa' })
                  .do({
                    right: (): void => fail(`Right side has not been expected`),
                    left: (error: AppError) => {
                      expect(error)
                        .toBeInstanceOf(InvalidInput);
                      expect(error.message)
                        .toEqual(ERROR_MESSAGE);
                    }
                  });
              });

              it(`address must be specify`, () => {
                const ERROR_MESSAGE: string = 'Validation failed: ["Missing mandatory property address"]';
                bodyValidator({ ...BODY, address: '' })
                  .do({
                    right: (): void => fail(`Right side has not been expected`),
                    left: (error: AppError) => {
                      expect(error)
                        .toBeInstanceOf(InvalidInput);
                      expect(error.message)
                        .toEqual(ERROR_MESSAGE);
                    }
                  });
              });

              it(`txId must be specify`, () => {
                const ERROR_MESSAGE: string = 'Validation failed: ["Missing mandatory property txId"]';
                bodyValidator({ ...BODY, txId: '' })
                  .do({
                    right: (): void => fail(`Right side has not been expected`),
                    left: (error: AppError) => {
                      expect(error)
                        .toBeInstanceOf(InvalidInput);
                      expect(error.message)
                        .toEqual(ERROR_MESSAGE);
                    }
                  });
              });

              it(`txIndex must be specify`, () => {
                const ERROR_MESSAGE: string = 'Validation failed: ["Missing mandatory property txIndex"]';
                bodyValidator({ ...BODY, txIndex: null })
                  .do({
                    right: (): void => fail(`Right side has not been expected`),
                    left: (error: AppError) => {
                      expect(error)
                        .toBeInstanceOf(InvalidInput);
                      expect(error.message)
                        .toEqual(ERROR_MESSAGE);
                    }
                  });
              });
            });
          });
        });
      });
    });
  });
});
