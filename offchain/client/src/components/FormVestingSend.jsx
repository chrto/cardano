import React, { useState } from 'react';
import './Form.css';
import utxoToLucid from '../utils/utxoToLucid';
import lucidStorage from '../utils/lucid/storage';
import dispatchData from '../utils/dispatchData';
import Modal from './Modal';
import getLocalDateTimeValue from '../utils/time/getLocalDateTimeValue';

function FormVestingSend({ scriptAddress, getSelectedWalletUtxos, deselectWalletUtxos }) {
  const [formData, setFormData] = useState({ valueAda: 3, beneficiary: '', deadline: getLocalDateTimeValue() });
  const [errorMessage, setErrorMessage] = useState(null);
  const [txHash, setTxHash] = useState(null);

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();
    // eslint-disable-next-line no-undef
    const amountLovelace = BigInt(formData.valueAda) * BigInt(1000000)
    const utxos = getSelectedWalletUtxos().map(utxoToLucid)
    // eslint-disable-next-line no-undef
    const deadline = BigInt(Date.parse(formData.deadline))
    const beneficiary = formData.beneficiary
    if (!beneficiary) {
      setErrorMessage('Beneficiary missing!')
      return null;
    }

    const options = {
      amountLovelace,
      contractAddress: scriptAddress,
      datum: {
        data: { beneficiary, deadline },
        type: 'vesting'
      }
    }

    lucidStorage.then(storage =>
      storage.buildPayToContractTx( utxos, options)
        .then(storage.signTx)
        .then(storage.submitTx)
        .then(dispatchData(setTxHash))
        .catch(dispatchData(setErrorMessage))
        .finally(() => {
          deselectWalletUtxos();
        })
    )
  };

  const handleReset = (e) => {
    e.preventDefault();
    deselectWalletUtxos();
    setFormData({...formData, valueAda: 3, beneficiary: '', deadline: getLocalDateTimeValue()});
  }

  const handleCloseModal = (e) => {
    e.preventDefault();

    setErrorMessage(null)
    setTxHash(null)
  }

  return (
    <div className="form">
      <div className="inputs">
        <label>Amount in ADA:</label>
        <input type="number" name="valueAda" value={formData.valueAda} onChange={handleChange} />

        <label>Beneficiary:</label>
        <input type="text" name="beneficiary" value={formData.beneficiary} onChange={handleChange} />

        <label>Deadline:</label>
        <input type="datetime-local" name="deadline" value={formData.deadline} onChange={handleChange} />
      </div>

      <div className="buttons">
        <button type="button" onClick={handleSubmit}>Submit</button>
        <button type="button" onClick={handleReset}>Reset</button>
      </div>

      <Modal isOpen={!!txHash || !!errorMessage} isError={!!errorMessage} onClose={handleCloseModal}>
        {
          !!txHash
            ? <div>
                <h2>Transaction has been submited.</h2>
                <p>Transaction Hash: {txHash}</p>
                <p><a className="form-link" href={`https://preview.cardanoscan.io/transaction/${txHash}`}>Open transaction in explorer</a></p>
              </div>
            : <div>
                <h2>Transaction has been failed.</h2>
                {!!errorMessage && errorMessage.split("\\n").map(line => <p>{line}</p> )}
              </div>
        }
      </Modal>
    </div>
  );
}

export default FormVestingSend;