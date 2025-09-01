import React, { useState } from 'react';
import './Form.css';
import utxoToLucid from '../utils/utxoToLucid';
import lucidStorage from '../utils/lucid/storage';
import Modal from './Modal';
import dispatchData from '../utils/dispatchData';
import getKeyUTxO from '../utils/getKeyUTxO';
import getOutRefFromUTxOKey from '../utils/getOutRefFromUTxOKey';

function FormFortyTwoCollect({ validatorScript, getSelectedScriptUtxos, getReferenceUtxo, deselect }) {
  const [formData, setFormData] = useState({ redeemerValue: 0 });
  const [error, setError] = useState(null);
  const [txHash, setTxHash] = useState(null);

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    const redeemer = Number(formData.redeemerValue)

    const utxos = getSelectedScriptUtxos().map(utxoToLucid)
    if (utxos.length === 0) {
      setError("No UTxO has been selected!");
      return null;
    }

    const references = utxos.filter(utxo => !!utxo.scriptHash).map(getKeyUTxO)
    if (references.length > 0) {
      setError(`Reference Script UTxOs has been selected to spend.\n[${references.join(', ')}]\nThis feature is not supported, Workig hard!`);
      return null;
    }

    const referenceKey = getReferenceUtxo()

    lucidStorage.then(storage =>
      !!referenceKey
        ? storage.getUTxOsByOutRef([getOutRefFromUTxOKey(referenceKey)])
            .then(utxoRefs => !!utxoRefs && utxoRefs.length>0
              ? Promise.resolve(utxoRefs[0])
              : Promise.reject(`UTxO at ${referenceKey} has not been found!`)
            )
            .then(utxoRef => !!utxoRef.scriptRef
              ? Promise.resolve(utxoRef)
              : Promise.reject(`UTxO at ${referenceKey} has no script reference!`)
            )
            .then(scriptRefUTxO => storage.buildSpendFromContractTx(utxos, { redeemer, redeemerType: 'integer', scriptRefUTxO }))
            .then(storage.signTx)
            .then(storage.submitTx)
            .then(dispatchData(setTxHash))
            .then(() => {
              resetForms()
            })
          .catch(dispatchData(setError))
        : storage.buildSpendFromContractTx(utxos, { redeemer, redeemerType: 'integer', script: validatorScript })
          .then(storage.signTx)
          .then(storage.submitTx)
          .then(dispatchData(setTxHash))
          .then(() => {
            resetForms()
          })
         .catch(dispatchData(setError))
    )
  };

  const resetForms = () => {
    deselect()
    setFormData({ ...formData, redeemerValue: 0 });
  }

  const handleReset = (e) => {
    e.preventDefault();
    resetForms()
  }

  const handleCloseModal = (e) => {
    e.preventDefault();

    setError(null)
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

      <Modal isOpen={!!txHash || !!error} isError={!!error} onClose={handleCloseModal}>
        {
          !!txHash
            ? <div>
              <h2>Transaction has been submited.</h2>
              <p>Transaction Hash: {txHash}</p>
              <p><a className="form-link" href={`https://preview.cardanoscan.io/transaction/${txHash}`}>Open transaction in explorer</a></p>
            </div>
            : <div>
                <h2>Transaction has been failed.</h2>
                {!!error && !! error.message ? error.message.split("\\n").map(line => <p>{line}</p>) : <p>{error}</p>}
              </div>
        }
      </Modal>
    </div>
  );
}

export default FormFortyTwoCollect;