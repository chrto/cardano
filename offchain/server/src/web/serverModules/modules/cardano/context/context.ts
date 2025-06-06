import { Context } from './context.types';
import { ContextFactory } from 'web/serverModules/types';

const contextFactory: ContextFactory<Context> = (req) => ({
  implicits: req.implicits
});
export default contextFactory;
