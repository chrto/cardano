import factoryParamsUnbound from './factoryParams.unbound';
import * as express from 'express';
import modelFactory from 'model/sequelize/modelFactory/modelFactory';
import serviceFactory from 'service/serviceFactory/serviceFactory';
import lucidStorageFactory from 'storage/cardano/lucid/lucid';

export default factoryParamsUnbound(
  express,
  lucidStorageFactory,
  serviceFactory,
  modelFactory
);
