import React, { useState } from 'react';
import '../App.css';
import { Data, Constr } from 'lucid-cardano';
import findUTxO from '../utils/findUTxO';
import utxoToLucid from '../utils/utxoToLucid';

const { giftScript } = require("../config.json");

function FormCollect({ title, scriptUtxos, lucid }) {
  const [formData, setFormData] = useState({ giftUtxo: '' });

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    const utxo = utxoToLucid(findUTxO(scriptUtxos, formData.giftUtxo))
    buildCardanoTx(utxo)
      .then(signCardanoTx)
      .then(tx => {
        console.log(tx.toString())
        return tx
      })
      .then(submitCardanoTx)
      .then(successHandler)
      .catch(errorHandler)
  };


  const buildCardanoTx = async utxo =>
    lucid
      .newTx()
      .collectFrom([utxo], Data.to(new Constr(0, [])))
      // .readFrom([collateralUtxo]) // 👈 specify it as collateral here !!! Check it !!!
      .attachSpendingValidator(giftScript)
      .complete()
      .catch(err => `Build transaction:\ninfo: ${err.info}\nmessage: ${err.message}`);

  const signCardanoTx = async tx =>
    tx
      .sign()
      .complete()
      .catch(err => `Sign transaction:\ninfo: ${err.info}\nmessage: ${err.message}`);

  const submitCardanoTx = async signedTx =>
    signedTx
      .submit()
      .catch(err => `Submit transaction:\ninfo: ${err.info}\nmessage: ${err.message}`);

  const successHandler = txId => alert(`Cardano tx submitted: https://preview.cardanoscan.io/transaction/${txId}`);
  const errorHandler = errMsg => alert(errMsg);

  return (
    <form onSubmit={handleSubmit} className="form">
      <h2>{title}</h2>
      <label>Gift UTxO:</label>
      <input type="text" name="giftUtxo" value={formData.name} onChange={handleChange} />

      <button type="submit">Submit</button>
    </form>
  );
}

export default FormCollect;