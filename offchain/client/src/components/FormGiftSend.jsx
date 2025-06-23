import React, { useState } from 'react';
import './Form.css';
import findUTxO from '../utils/findUTxO';
import utxoToLucid from '../utils/utxoToLucid';
import lucidStorage from '../utils/lucid/storage';

function FormGiftSend({ title, scriptAddress, walletUtxos }) {
  const [formData, setFormData] = useState({ giftValue: 3, utxoWithIndex: '' });

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();
    // eslint-disable-next-line no-undef
    const amountLovelace = BigInt(formData.giftValue) * BigInt(1000000)
    const utxoRef = formData.utxoWithIndex

    lucidStorage.then(storage =>
      !!utxoRef
        ? storage.buildPayToContractTxFromUtxo(amountLovelace, utxoToLucid(findUTxO(walletUtxos, utxoRef)), scriptAddress)
          .then(storage.signTx)
          .then(storage.submitTx)
          .then(storage.successHandler)
          .catch(storage.errorHandler)
        : storage.buildPayToContractTx(amountLovelace, scriptAddress)
          .then(storage.signTx)
          .then(storage.submitTx)
          .then(storage.successHandler)
          .catch(storage.errorHandler)
    )
  };

  return (
    <form onSubmit={handleSubmit} className="form">
      <h2>{title}</h2>
      <label>Value in ADA:</label>
      <input type="number" name="giftValue" value={formData.giftValue} onChange={handleChange} />
      <label>UTxO (Optional):</label>
      <input type="text" name="utxoWithIndex" value={formData.utxoWithIndex} onChange={handleChange} />

      <button type="submit">Submit</button>
    </form>
  );
}

export default FormGiftSend;