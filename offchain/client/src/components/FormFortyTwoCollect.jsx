import React, { useState } from 'react';
import './Form.css';
import utxoToLucid from '../utils/utxoToLucid';
import lucidStorage from '../utils/lucid/storage';

function FormFortyTwoCollect({ validatorScript, getSelectedScriptUtxos, deselectScriptUtxos }) {
  const [formData, setFormData] = useState({ redeemerValue: 0 });

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    const redeemer = Number(formData.redeemerValue)
    const utxos = getSelectedScriptUtxos().map(utxoToLucid)
    if (utxos.length === 0) {
      return alert("No UTxO has been selected!");
    }

    lucidStorage.then(storage =>
      storage.buildSpendFromContractTx(validatorScript, utxos, redeemer, 'integer')
        .then(storage.signTx)
        .then(storage.submitTx)
        .then(storage.successHandler)
        .then(() => {
          resetForms()
        })
        .catch(storage.errorHandler)
    )
  };

  const resetForms = () => {
    deselectScriptUtxos()
    setFormData({ ...formData, redeemerValue: 0 });
  }

  const handleReset = (e) => {
    e.preventDefault();
    resetForms()
  }

  return (
    <div className="form">
      <label>Redeemer value:</label>
      <input type="number" name="redeemerValue" value={formData.redeemerValue} onChange={handleChange} />

      <button type="button" onClick={handleSubmit}>Submit</button>
      <button type="button" onClick={handleReset}>Reset</button>
    </div>
  );
}

export default FormFortyTwoCollect;