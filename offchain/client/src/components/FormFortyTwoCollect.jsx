import React, { useState } from 'react';
import './Form.css';
import findUTxO from '../utils/findUTxO';
import utxoToLucid from '../utils/utxoToLucid';
import lucidStorage from '../utils/lucid/storage';

function FormFortyTwoCollect({ title, scriptUtxos, validatorScript }) {
  const [formData, setFormData] = useState({ giftUtxo: '', redeemerValue: 0 });

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    const utxo = utxoToLucid(findUTxO(scriptUtxos, formData.giftUtxo))
    const redeemer = Number(formData.redeemerValue)

    lucidStorage.then(storage =>
      storage.buildSpendFromContractTx(validatorScript, utxo, redeemer)
        .then(storage.signTx)
        .then(storage.submitTx)
        .then(storage.successHandler)
        .catch(storage.errorHandler)
    )
  };

  return (
    <form onSubmit={handleSubmit} className="form">
      <h2>{title}</h2>
      <label>Gift UTxO:</label>
      <input type="text" name="giftUtxo" value={formData.name} onChange={handleChange} />
      <input type="number" name="redeemerValue" value={formData.redeemerValue} onChange={handleChange} />

      <button type="submit">Submit</button>
    </form>
  );
}

export default FormFortyTwoCollect;