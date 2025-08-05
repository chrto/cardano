import React, { useState } from 'react';
import './Form.css';
import Modal from './Modal';
// import postDataToServer from '../utils/postDataToServer';
// import dispatchData from '../utils/dispatchData';
import lucidStorage from '../utils/lucid/storage';
import dispatchData from '../utils/dispatchData';

function FormScriptDeploy({ getSelectedScript, deselectScript}) {
  const [formData, setFormData] = useState({ address: ''});
  const [errorMessage, setErrorMessage] = useState(null);
  const [result, setResult] = useState(null);
  const [txHash, setTxHash] = useState(null);

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    const script = getSelectedScript()
    const address = formData.address

    if (!script) {
      setErrorMessage({
        message: 'Build',
        details: 'Script has not been selected!'
      })
      return null
    }
    if (!address) {
      setErrorMessage({
        message: 'Build',
        details: 'Address has not been selected!'
      })
      return null
    }
    // setResult({ address, type: script.type, script: script.script })
    const options = {
      contractAddress: address,
      scriptRef: {
        type: script.type,
        script: script.script
      }
    }
    const utxos = [] // TODO
    lucidStorage.then(storage =>
      storage.buildPayToContractTx( utxos, options)
        .then(storage.signTx)
        .then(storage.submitTx)
        .then(dispatchData(setTxHash))
        .catch(dispatchData(setErrorMessage))
    )
  };

  const handleReset = (e) => {
    e.preventDefault();
    setFormData({ ...formData, address: '' });
    deselectScript()
  }

  const handleCloseModal = (e) => {
    e.preventDefault();

    setErrorMessage(null)
    setResult(null)
    setTxHash(null)
  }

  return (
    <div className="form">
      <div className="inputs">
        <label>Address:</label>
        <input type="text" name="address" value={formData.address} onChange={handleChange} />

      </div>

      <div className="buttons">
        <button type="button" onClick={handleSubmit}>Submit</button>
        <button type="button" onClick={handleReset}>Reset</button>
      </div>

      <Modal isOpen={!!result || !!errorMessage || !!txHash} isError={!!errorMessage} onClose={handleCloseModal}>
        {
          !!result &&
            <div>
              <h2>Script has been deployed.</h2>
              {Object.keys(result).map(key => <p kye={key}>{ key }: {result[key]}</p>)}
            </div>
        }
        {
          !!errorMessage &&
            <div>
              <h2>Transaction has been failed.</h2>
              {!!errorMessage.message && <p>{ errorMessage.message}</p>}
              {!!errorMessage.details && <p>{ errorMessage.details}</p>}
            </div>
        }
        {
          !!txHash &&
            <div>
              <h2>Transaction has been submited.</h2>
              <p>Transaction Hash: {txHash}</p>
              <p><a className="form-link" href={`https://preview.cardanoscan.io/transaction/${txHash}`}>Open transaction in explorer</a></p>
            </div>
        }
      </Modal>
    </div>
  );
}

export default FormScriptDeploy;