import React, { useState } from 'react';
import '../App.css';
import { Data, Constr } from 'lucid-cardano';
import findUTxO from '../utils/findUTxO';
import utxoToLucid from '../utils/utxoToLucid';
import sniff from '../utils/sniff';

function FormSend({ title, giftAddress, walletUtxos, lucid }) {
  const [formData, setFormData] = useState({ giftValue: 3, utxoWithIndex: '' });

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();
    // eslint-disable-next-line no-undef
    const amountLovelace = BigInt(formData.giftValue) * BigInt(1000000)
    const utxoRef = formData.utxoWithIndex

    if (!!utxoRef) {

      buildCardanoTxFromUtxo(amountLovelace, utxoToLucid(findUTxO(walletUtxos, utxoRef)))
        .then(signCardanoTx)
        .then(submitCardanoTx)
        .then(successHandler)
        .catch(errorHandler)
    } else {
      buildCardanoTx(amountLovelace)
        .then(sniff("Transaction"))
        .then(signCardanoTx)
        .then(sniff("Transaction"))
        .then(submitCardanoTx)
        .then(successHandler)
        .catch(errorHandler)
    }
  };

  const buildCardanoTxFromUtxo = async (amountLovelace, utxo) =>
    lucid
      .newTx()
      .collectFrom([utxo])
      .payToContract(giftAddress, { inline: Data.to(new Constr(0, [])) }, { lovelace: amountLovelace })
      .complete()
      .catch(err => `Build transaction:\ninfo: ${err.info}\nmessage: ${err.message}`);

  const buildCardanoTx = async (amountLovelace) =>
    lucid
      .newTx()
      .payToContract(giftAddress, { inline: Data.to(new Constr(0, [])) }, { lovelace: amountLovelace })
      .complete()
      .catch(err => `Build transaction:\ninfo: ${err.info}\nmessage: ${err.message}`);

  const signCardanoTx = async tx =>
    tx
      .sign()
      .complete()
      .catch(err => `Sign transaction:\ninfo: ${err.info}\nmessage: ${err.message}`);

  const submitCardanoTx = async signedTx =>
    signedTx.submit()
      .catch(err => `Submit transaction:\ninfo: ${err.info}\nmessage: ${err.message}`);

  const successHandler = txId => alert("Cardano tx submitted: " + txId);
  const errorHandler = err => alert(`Cardano transaction:\ninfo: ${err.info}\nmessage: ${err.message}`);

  return (
    <form onSubmit={handleSubmit} className="form">
      <h2>{title}</h2>
      {/* <text>Script address: {giftAddress}</text> */}
      <label>Value in ADA:</label>
      <input type="number" name="giftValue" value={formData.giftValue} onChange={handleChange} />
      <label>UTxO (Optional):</label>
      <input type="text" name="utxoWithIndex" value={formData.utxoWithIndex} onChange={handleChange} />

      <button type="submit">Submit</button>
    </form>
  );
}

export default FormSend;