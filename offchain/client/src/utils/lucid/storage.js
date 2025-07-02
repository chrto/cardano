import { Lucid, Blockfrost, Kupmios } from 'lucid-cardano';
import { toCBOR } from './data';

const { provider, wallet } = require("../../config.json");

const getPorvider =
  provider.use === "blockFrost"
    ? new Blockfrost(provider[provider.use].url, provider[provider.use].projectId)
    : provider.use === "node" | provider.use === "node_local"
      ? new Kupmios(provider[provider.use].kupo, provider[provider.use].ogmios)
      : null

const setWallet = async (lucid) =>
  wallet.use === "privateKey"
    ? Promise.resolve(lucid.selectWalletFromPrivateKey(wallet[wallet.use]))
      .then(_ => lucid)
    : window.cardano[wallet[wallet.use]].enable()
      .then(api => lucid.selectWallet(api))
      .then(_ => lucid)

const loadCardano = async () =>
  Lucid.new(getPorvider, provider.network)
    .then(setWallet)

function LucidStorage(lucid) {
  this.lucid = lucid;

  this.getWalletAddress = this.getWalletAddress.bind(this);
  this.buildPayToContractTxFromUtxo = this.buildPayToContractTxFromUtxo.bind(this);
  this.buildPayToContractTx = this.buildPayToContractTx.bind(this);
  this.buildSpendFromContractTx = this.buildSpendFromContractTx.bind(this);
  this.signTx = this.signTx.bind(this);
  this.submitTx = this.submitTx.bind(this);
  this.successHandler = this.successHandler.bind(this);
  this.errorHandler = this.errorHandler.bind(this);
}

LucidStorage.prototype.getWalletAddress = async function () {
  return this.lucid.wallet.address()
}

LucidStorage.prototype.buildPayToContractTxFromUtxo = async function (amountLovelace, utxo, contractAddress, datum = null) {
  const datumCBOR = toCBOR(datum)

  return this.lucid
    .newTx()
    .collectFrom([utxo])
    .payToContract(contractAddress, { inline: datumCBOR }, { lovelace: amountLovelace })
    .complete()
    .catch(err => {
      console.error(err)
      throw new Error(`Build transaction:\ninfo: ${JSON.stringify(err)}`)
    });
}

LucidStorage.prototype.buildPayToContractTx = async function (amountLovelace, contractAddress, datum = null) {

  const datumCBOR = toCBOR(datum)

  return this.lucid
    .newTx()
    .payToContract(contractAddress, { inline: datumCBOR }, { lovelace: amountLovelace })
    .complete()
    .catch(err => {
      console.error(err)
      throw new Error(`Build transaction:\ninfo: ${JSON.stringify(err)}`)
    });
}

LucidStorage.prototype.buildSpendFromContractTx = async function (script, utxo, redeemer = null) {

  const redeemerCOBR = toCBOR(redeemer)

  return this.lucid
    .newTx()
    .collectFrom([utxo], redeemerCOBR)
    // .readFrom([collateralUtxo]) // 👈 specify it as collateral here !!! Check it !!!
    .attachSpendingValidator(script)
    .complete()
    .catch(err => {
      console.error(err)
      throw new Error(`Build transaction:\nOrigin error: ${JSON.stringify(err)}`)
    });
}

LucidStorage.prototype.signTx = async function (tx) {
  return tx
    .sign()
    .complete()
    .catch(err => {
      throw new Error(`Sign transaction:\ninfo: ${JSON.stringify(err)}`)
    });
}

LucidStorage.prototype.submitTx = async function (signedTx) {
  return signedTx
    .submit()
    .catch(err => {
      throw new Error(`Submit transaction:\ninfo: ${JSON.stringify(err)}`)
    });
}

LucidStorage.prototype.successHandler = function (txId) {
  return alert(`Cardano tx submitted: https://preview.cardanoscan.io/transaction/${txId}`);
}

LucidStorage.prototype.errorHandler = function (errMsg) {
  return alert(errMsg);
}

const lucidStorage = loadCardano()
  .then(lucid => new LucidStorage(lucid));

export default lucidStorage;