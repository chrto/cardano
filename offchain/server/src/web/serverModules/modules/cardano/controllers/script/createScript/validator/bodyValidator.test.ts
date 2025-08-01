import bodyValidator from './bodyValidator';

import { ScriptBody } from '../createScript.types';
import { AppError } from 'common/error';
import { InvalidInput } from 'common/httpErrors';
import { PlutusVersion, ScriptType } from 'model/cardano/cardano.types';
import { ScritpCategory } from 'model/sequelize/model/script/script.types';

const BODY: ScriptBody = {
  type: PlutusVersion.PlutusV2,
  script: '49480100002221200101',
  category: ScritpCategory.Gift,
  title: 'PPP',
  description: 'Example of gift script from PPP'
};

describe('Web Server', () => {
  describe('Modules', () => {
    describe('Portal', () => {
      describe('controller', () => {
        describe('script controller', () => {
          describe('create script', () => {
            describe('body validator', () => {
              it(`Should return Either with body object in right side`, () => {
                bodyValidator(BODY)
                  .do({
                    right: (body: ScriptBody) => {
                      expect(body).toBeObject;
                      expect(body).toStrictEqual(BODY);
                    },
                    left: (error: AppError) => fail(`Left side has not been expected: ${error.message}`)
                  });
              });

              it(`Should have mandatory fields`, () => {
                const ERROR_MESSAGE: string = 'following required properties are missing in request: type, script, category, title, description';
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

              it(`type must be specify`, () => {
                const ERROR_MESSAGE: string = 'Validation failed: ["Missing mandatory property type"]';
                bodyValidator({ ...BODY, type: '' as ScriptType })
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

              it(`type must be value from 'PlutusVersion' or 'ScriptTypeExt' enum`, () => {
                const ERROR_MESSAGE: string = 'Validation failed: ["Invalid script type"]';
                bodyValidator({ ...BODY, type: 'aaaa' as ScriptType })
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

              it(`script must be specify`, () => {
                const ERROR_MESSAGE: string = 'Validation failed: ["Missing mandatory property script"]';
                bodyValidator({ ...BODY, script: '' })
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

              it(`category must be specify`, () => {
                const ERROR_MESSAGE: string = 'Validation failed: ["Missing mandatory property category"]';
                bodyValidator({ ...BODY, category: '' as ScritpCategory })
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

              it(`category must be value from 'ScritpCategory' enum`, () => {
                const ERROR_MESSAGE: string = 'Validation failed: ["Invalid script category"]';
                bodyValidator({ ...BODY, category: 'aaaa' as ScritpCategory })
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

              it(`title must be specify`, () => {
                const ERROR_MESSAGE: string = 'Validation failed: ["Missing mandatory property title"]';
                bodyValidator({ ...BODY, title: '' })
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

              it(`description must be specify`, () => {
                const ERROR_MESSAGE: string = 'Validation failed: ["Missing mandatory property description"]';
                bodyValidator({ ...BODY, description: '' })
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
