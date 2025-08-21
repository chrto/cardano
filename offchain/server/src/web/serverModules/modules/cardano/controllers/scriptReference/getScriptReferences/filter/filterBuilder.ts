import { WhereOptions } from 'sequelize';
import { Query } from '../getScriptReferences.types';

export default (query: Query): WhereOptions => ({
  ... !!query.scriptId && { scriptId: query.scriptId },
  ... !!query.address && { address: query.address },
  ... !!query.unspend && { unspend: query.unspend }
});
