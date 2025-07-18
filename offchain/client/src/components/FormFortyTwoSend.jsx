import React, { useState } from 'react';
import './Form.css';
import findUTxO from '../utils/findUTxO';
import utxoToLucid from '../utils/utxoToLucid';
import lucidStorage from '../utils/lucid/storage';

function FormFortyTwoSend({ title, scriptAddress, walletUtxos }) {
  const [formData, setFormData] = useState({ valueAda: 3, utxoWithIndex: '' });

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();
    // eslint-disable-next-line no-undef
    const amountLovelace = BigInt(formData.valueAda) * BigInt(1000000)
    const utxoRef = formData.utxoWithIndex

    lucidStorage.then(storage =>
      !!utxoRef
        ? storage.buildPayToContractTxFromUtxo(amountLovelace, utxoToLucid(findUTxO(walletUtxos, utxoRef)), scriptAddress)
          .then(storage.signTx)
          .then(storage.submitTx)
          .then(storage.successHandler)
          .catch(storage.errorHandler)
          .finally(() => {
            setFormData({...formData, utxoWithIndex: '', valueAda: 3})
          })
        : storage.buildPayToContractTx(amountLovelace, scriptAddress)
          .then(storage.signTx)
          .then(storage.submitTx)
          .then(storage.successHandler)
          .catch(storage.errorHandler)
          .finally(() => {
            setFormData({...formData, utxoWithIndex: '', valueAda: 3})
          })
    )
  };

  const handleReset = (e) => {
    e.preventDefault();
    setFormData({...formData, utxoWithIndex: '', valueAda: 3});
  }

  return (
    <div className="form">
      <h2>{title}</h2>
      <label>Value in ADA:</label>
      <input type="number" name="valueAda" value={formData.valueAda} onChange={handleChange} />
      <label>UTxO (Optional):</label>
      <input type="text" name="utxoWithIndex" value={formData.utxoWithIndex} onChange={handleChange} />

      <button type="button" onClick={handleSubmit}>Submit</button>
      <button type="button" onClick={handleReset}>Reset</button>
    </div>
  );
}

export default FormFortyTwoSend;