import { useState } from 'react';
import './Form.css';
import utxoToLucid from '../utils/utxoToLucid';
import lucidStorage from '../utils/lucid/storage';

function FormFortyTwoSend({ scriptAddress, getSelectedWalletUtxos, deselectWalletUtxos }) {
  const [formData, setFormData] = useState({ valueAda: 3 });

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();
    // eslint-disable-next-line no-undef
    const amountLovelace = BigInt(formData.valueAda) * BigInt(1000000)
    const utxos = getSelectedWalletUtxos().map(utxoToLucid)

    lucidStorage.then(storage =>
      storage.buildPayToContractTxFromUtxo(amountLovelace, utxos, scriptAddress)
        .then(storage.signTx)
        .then(storage.submitTx)
        .then(storage.successHandler)
        .catch(storage.errorHandler)
        .finally(() => {
          deselectWalletUtxos();
          setFormData({...formData, valueAda: 3})
        })
    )
  };

  const handleReset = (e) => {
    e.preventDefault();

    deselectWalletUtxos();
    setFormData({...formData, valueAda: 3});
  }

  return (
    <div className="form">
      <div className="inputs">
        <label>Value in ADA:</label>
        <input type="number" name="valueAda" value={formData.valueAda} onChange={handleChange} />
      </div>
      <div className="buttons">
        <button type="button" onClick={handleSubmit}>Submit</button>
        <button type="button" onClick={handleReset}>Reset</button>
      </div>
    </div>
  );
}

export default FormFortyTwoSend;