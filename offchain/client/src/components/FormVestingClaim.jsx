import React, { useState } from 'react';
import './Form.css';
import utxoToLucid from '../utils/utxoToLucid';
import lucidStorage from '../utils/lucid/storage';
import Modal from './Modal';
import dispatchData from '../utils/dispatchData';
import getLocalDateTimeValue from '../utils/time/getLocalDateTimeValue';

function FormVestingClaim({ publicKeyHash, validatorScript, getSelectedScriptUtxos, deselectScriptUtxos }) {
  const [formData, setFormData] = useState({ validFrom: getLocalDateTimeValue() });
  const [errorMessage, setErrorMessage] = useState(null);
  const [txHash, setTxHash] = useState(null);

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    const utxos = getSelectedScriptUtxos().map(utxoToLucid)
    const validFrom = Date.parse(formData.validFrom)

    if (utxos.length === 0) {
      setErrorMessage("No UTxO has been selected!");
      return null;
    }

    lucidStorage.then(storage =>
      storage.buildSpendFromContractTx(utxos, { publicKeyHash, validFrom, script: validatorScript })
        .then(storage.signTx)
        .then(storage.submitTx)
        .then(dispatchData(setTxHash))
        .catch(e => {
          setErrorMessage(e.message)
        })
    )
  };

  const resetForms = () => {
    setFormData({...formData, validFrom: getLocalDateTimeValue() })
    deselectScriptUtxos()
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
        <label>Valid From:</label>
        <input type="datetime-local" name="validFrom" value={formData.validFrom} onChange={handleChange} />
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
              <p>{errorMessage}</p>
              </div>
        }
      </Modal>
    </div>
  );
}

export default FormVestingClaim;