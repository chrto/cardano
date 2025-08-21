import { EDatabaseDialect } from 'web/server/configuration/loader/database/databaseConfig.types';
import { Options, Sequelize } from 'sequelize';
import initScriptReferenceModel, { ScriptReference } from '../scriptReference/scriptReference';
import initScriptModel, { Script } from './scirpt';
import { ScriptItems, ScritpCategory } from './script.types';
import { PlutusVersion } from 'model/cardano/cardano.types';
import { ScriptReferenceItems } from '../scriptReference/scriptReference.types';
import { SequelizeIncludes } from 'service/sequelize/types';

const SEQUELIZE_CONFIG: Options = {
  dialect: EDatabaseDialect.sqlite,
  storage: ':memory:',
  logging: false  // Enable debug logging here
};

const SCRIPT_REFERENCE_ITEMS: ScriptReferenceItems = {
  id: 'f923b2c9-ffcf-4a0a-bdc9-a4a4ae2a687e',
  scriptId: null,
  address: 'addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla',
  txId: '82e75104c2ffcab389fae6a9c87ebbe99e83cd7826d02534e77783b12c62e467',
  txIndex: 0,
  unspend: true
};

const SCRIPT_ITEMS: ScriptItems = {
  id: 'f0962fc9-882d-416d-bc08-fed1d5aa3a36',
  type: PlutusVersion.PlutusV2,
  script: '49480100002221200101',
  category: ScritpCategory.Gift,
  title: 'PPP',
  description: 'Example of gift script from PPP'
};

const INCLUDES: SequelizeIncludes = {
  include: [
    {
      model: ScriptReference,
      as: 'scriptReferences'
    }
  ]
};

const sequelize = new Sequelize(SEQUELIZE_CONFIG);
initScriptModel(sequelize);
initScriptReferenceModel(sequelize);
Script.hasMany(ScriptReference, { as: 'scriptReferences', foreignKey: 'scriptId' });
ScriptReference.belongsTo(Script, { as: 'script', foreignKey: 'scriptId' });

sequelize.sync({ force: true })
  .then(_ => Script.create(SCRIPT_ITEMS))
  .then(_ => Script.findOne({ where: { id: SCRIPT_ITEMS.id }, ...INCLUDES }))
  .then(script =>
    script.createScriptReference(SCRIPT_REFERENCE_ITEMS)
  )
  .then(async _ => {
    const scripts = await Script.findAll({ ...INCLUDES });
    const refs = await ScriptReference.findAll();

    if (scripts.length !== 1) {
      return Promise.reject(`Sholud be exact one Script. Has ${scripts.length}`);
    }

    if (scripts[0].scriptReferences.length !== 1) {
      return Promise.reject(`Sholud be exact one ScriptReference on Script. Has ${scripts.length}`);
    }

    console.log(scripts.map(s => s.get({ plain: true })));
    console.log(scripts[0].scriptReferences[0].get({ plain: true }));

    console.log(refs.map(r => r.get({ plain: true })));
    return Promise.resolve(null);
  })
  .catch(msg =>
    console.error(`!!!!!!!!!!!!! ${msg} !!!!!!!!!!!!!`)
  );
