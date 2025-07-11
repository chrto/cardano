import React, { useState } from 'react';
import './Form.css';
import findUTxO from '../utils/findUTxO';
import utxoToLucid from '../utils/utxoToLucid';
import lucidStorage from '../utils/lucid/storage';

function FormFortyTwoCollect({ title, scriptUtxos, validatorScript }) {
  const [formData, setFormData] = useState({ utxoWithIndex: '', redeemerValue: 0 });

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    const utxo = utxoToLucid(findUTxO(scriptUtxos, formData.utxoWithIndex))
    const redeemer = Number(formData.redeemerValue)

    lucidStorage.then(storage =>
      storage.buildSpendFromContractTx(validatorScript, utxo, redeemer, 'integer')
        .then(storage.signTx)
        .then(storage.submitTx)
        .then(storage.successHandler)
        .catch(storage.errorHandler)
    )
  };

  const handleReset = (e) => {
    e.preventDefault();
    setFormData({ ...formData, utxoWithIndex: '', redeemerValue: 0 });
  }

  return (
    <div className="form">
      <h2>{title}</h2>
      <label>Gift UTxO:</label>
      <input type="text" name="utxoWithIndex" value={formData.utxoWithIndex} onChange={handleChange} />
      <label>Redeemer value:</label>
      <input type="number" name="redeemerValue" value={formData.redeemerValue} onChange={handleChange} />

      <button type="button" onClick={handleSubmit}>Submit</button>
      <button type="button" onClick={handleReset}>Reset</button>
    </div>
  );
}

export default FormFortyTwoCollect;