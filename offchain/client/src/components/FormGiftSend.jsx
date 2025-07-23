import { useState } from 'react';
import './Form.css';
import utxoToLucid from '../utils/utxoToLucid';
import lucidStorage from '../utils/lucid/storage';

function FormGiftSend({ scriptAddress, getSelectedWalletUtxos, deselectWalletUtxos }) {
  const [formData, setFormData] = useState({ giftValue: 3 });

  const handleChange = (e) => {
    e.preventDefault();

    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();
    // eslint-disable-next-line no-undef
    const amountLovelace = BigInt(formData.giftValue) * BigInt(1000000)
    const utxos = getSelectedWalletUtxos().map(utxoToLucid)

    lucidStorage.then(storage =>
      storage.buildPayToContractTxFromUtxo(amountLovelace, utxos, scriptAddress)
        .then(storage.signTx)
        .then(storage.submitTx)
        .then(storage.successHandler)
        .catch(storage.errorHandler)
        .finally(() => {
          deselectWalletUtxos();
          setFormData({ ...formData, giftValue: 3 });
        })
    )
  };

  const handleReset = (e) => {
    e.preventDefault();

    deselectWalletUtxos();
    setFormData({...formData, giftValue: 3});
  }

  return (
    <div className="form">
      <label>Value in ADA:</label>
      <input type="number" name="giftValue" value={formData.giftValue} onChange={handleChange} />

      <button type="button" onClick={handleSubmit}>Submit</button>
      <button type="button" onClick={handleReset}>Reset</button>
    </div>
  );
}

export default FormGiftSend;