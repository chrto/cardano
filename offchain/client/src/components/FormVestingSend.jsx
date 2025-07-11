import React, { useState, useEffect } from 'react';
import './Form.css';
import findUTxO from '../utils/findUTxO';
import utxoToLucid from '../utils/utxoToLucid';
import lucidStorage from '../utils/lucid/storage';

function FormVestingSend({ title, scriptAddress, walletUtxos }) {
  const [formData, setFormData] = useState({ valueAda: 3, beneficiary: '', deadline: '', utxoWithIndex: '' });

    useEffect(() => {
      setFormData({...formData, deadline: getLocalDateTimeValue()})
    }, []);

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const getLocalDateTimeValue = () => {
    const now = new Date();
    const year = now.getFullYear();
    const month = String(now.getMonth() + 1).padStart(2, '0');
    const day = String(now.getDate()).padStart(2, '0');
    const hours = String(now.getHours()).padStart(2, '0');
    const minutes = String(now.getMinutes()).padStart(2, '0');

    return `${year}-${month}-${day}T${hours}:${minutes}`;
  }
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
      <label>Amount in ADA:</label>
      <input type="number" name="valueAda" value={formData.valueAda} onChange={handleChange} />

      <label>Beneficiary:</label>
      <input type="text" name="beneficiary" value={formData.beneficiary} onChange={handleChange} />

      <label>Deadline:</label>
      <input type="datetime-local" name="deadline" value={formData.deadline} onChange={handleChange} />

      <label>UTxO (Optional):</label>
      <input type="text" name="utxoWithIndex" value={formData.utxoWithIndex} onChange={handleChange} />

      <button type="button" onClick={handleSubmit}>Submit</button>
      <button type="button" onClick={handleReset}>Reset</button>
    </div>
  );
}

export default FormVestingSend;