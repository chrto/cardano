import React, { useState } from 'react';
import './Form.css';
import utxoToLucid from '../utils/utxoToLucid';
import lucidStorage from '../utils/lucid/storage';
import Modal from './Modal';
import dispatchData from '../utils/dispatchData';

function FormFortyTwoCollect({ validatorScript, getSelectedScriptUtxos, deselectScriptUtxos }) {
  const [formData, setFormData] = useState({ redeemerValue: 0 });
  const [errorMessage, setErrorMessage] = useState(null);
  const [txHash, setTxHash] = useState(null);

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    const redeemer = Number(formData.redeemerValue)
    const utxos = getSelectedScriptUtxos().map(utxoToLucid)
    if (utxos.length === 0) {
      setErrorMessage("No UTxO has been selected!");
      return null;
    }

    const options = { redeemer, redeemerType: 'integer' }

    lucidStorage.then(storage =>
      storage.buildSpendFromContractTx(validatorScript, utxos, options)
        .then(storage.signTx)
        .then(storage.submitTx)
        .then(dispatchData(setTxHash))
        .then(() => {
          resetForms()
        })
        .catch(e => {
          setErrorMessage(e.message)
        })
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

  const handleCloseModal = (e) => {
    e.preventDefault();

    setErrorMessage(null)
    setTxHash(null)
  }

  return (
    <div className="form">
      <div className="inputs">
        <label>Redeemer value:</label>
        <input type="number" name="redeemerValue" value={formData.redeemerValue} onChange={handleChange} />
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

export default FormFortyTwoCollect;