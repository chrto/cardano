import { WhereOptions } from 'sequelize';
import { Query } from '../getScripts.types';

export default (query: Query): WhereOptions => ({
  ... !!query.category && { category: query.category },
  ... !!query.type && { type: query.type }
});
