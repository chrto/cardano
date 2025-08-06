import { useState } from 'react';
import './Form.css';
import utxoToLucid from '../utils/utxoToLucid';
import lucidStorage from '../utils/lucid/storage';
import Modal from './Modal';
import dispatchData from '../utils/dispatchData';
import getOutRefFromUTxOKey from '../utils/getOutRefFromUTxOKey';

function FormGiftCollect({ validatorScript, getSelectedScriptUtxos, deselectScriptUtxos}) {
  const [errorMessage, setErrorMessage] = useState(null);
  const [txHash, setTxHash] = useState(null);
  const [formData, setFormData] = useState({ scriptRef: '' });

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    const utxos = getSelectedScriptUtxos().map(utxoToLucid)
    if (utxos.length === 0) {
      setErrorMessage("No UTxO has been selected!");
      return null;
    }

    lucidStorage.then(storage =>
      formData.scriptRef !== ''
        ? storage.getUTxOsByOutRef([getOutRefFromUTxOKey(formData.scriptRef)])
            .then(utxoRefs => !!utxoRefs && utxoRefs.length>0
              ? Promise.resolve(utxoRefs)
              : Promise.reject(`UTxO at ${formData.scriptRef} has not been found!`)
            )
            .then(utxoRefs => utxoRefs.length === 1
              ? Promise.resolve(utxoRefs[0])
              : Promise.reject(`Expected exactly one UTxO at ${formData.scriptRef}!`)
            )
            .then(utxoRef => !!utxoRef.scriptRef
              ? Promise.resolve(utxoRef)
              : Promise.reject(`UTxO at ${formData.scriptRef} has no script reference!`)
            )
            .then(utxoRef => storage.buildSpendFromContractTx(utxos, { scriptRefUTxO: utxoRef }))
            .then(storage.signTx)
            .then(storage.submitTx)
            .then(dispatchData(setTxHash))
            .then(() => {
              resetForms()
            })
          .catch(dispatchData(setErrorMessage))
        : storage.buildSpendFromContractTx(utxos, { script: validatorScript })
          .then(storage.signTx)
          .then(storage.submitTx)
          .then(dispatchData(setTxHash))
          .then(() => {
            resetForms()
          })
          .catch(dispatchData(setErrorMessage))
    )
  };

  const resetForms = () => {
    deselectScriptUtxos()
    setFormData({ scriptRef: '' });
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
        <label>{"Reference to script (Optional):"}</label>
        <input type="text" name="scriptRef" value={formData.scriptRef} onChange={handleChange} />
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

export default FormGiftCollect;