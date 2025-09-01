import React, { useState } from 'react';
import './Form.css';
import Modal from './Modal';
import lucidStorage from '../utils/lucid/storage';
import dispatchData from '../utils/dispatchData';
import postDataToServer from '../utils/postDataToServer';
import getData from '../utils/getDataFromServer';
import utxoToLucid from '../utils/utxoToLucid';

function FormScriptDeploy({ getSelectedScript, deselectScript, getSelectedWalletUtxos, deselectWalletUtxos }) {
  const [formData, setFormData] = useState({ address: ''});
  const [error, setError] = useState(null);
  const [txHash, setTxHash] = useState(null);
  const [refId, setRefId] = useState(null);

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    const script = getSelectedScript()
    const address = formData.address
    const utxos = getSelectedWalletUtxos().map(utxoToLucid)

    if (!script) {
      setError({
        message: 'Build',
        details: 'Script has not been selected!'
      })
      return null
    }
    if (!address) {
      setError({
        message: 'Build',
        details: 'Address has not been selected!'
      })
      return null
    }

    const options = {
      contractAddress: address,
      scriptRef: {
        type: script.type,
        script: script.script
      }
    }

    setTxHash(null)
    setRefId(null)

    lucidStorage.then(storage =>
      storage.buildPayToContractTx(utxos, options)
        .then(storage.signTx)
        .then(storage.submitTx)
        .then(dispatchData(setTxHash))
        .then(getDeploymentTx(address))
        .then(storeDeployment(script, address))
        .then(ref => ref.id)
        .then(dispatchData(setRefId))
        .catch(dispatchData(setError))
    )
  };

  function delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms))
  }

  const getDeploymentTx = address => txHash =>
    getData(`cardano/${address}/utxos`)
      .then(utxos => {
        const deployment = utxos.find(utxo => utxo.txId === txHash && utxo.address === address)
        return !!deployment
          ? Promise.resolve(deployment)
          : delay(5000).then(_ => getDeploymentTx(address)(txHash))
      })

  const storeDeployment = (script, address) => tx =>
    postDataToServer(`cardano/scriptReferences/`, {
      scriptId: script.id,
      address,
      txId: tx.txId,
      txIndex: tx.txIndex
    })

  const handleReset = (e) => {
    e.preventDefault();
    setFormData({ ...formData, address: '' });
    deselectScript()
    deselectWalletUtxos()
  }

  const handleCloseModal = (e) => {
    e.preventDefault();

    setError(null)
    setTxHash(null)
    setRefId(null)
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

      <Modal isOpen={!!error || !!txHash} isError={!!error} onClose={handleCloseModal}>
        {
          !!error &&
            <div>
              <h2>Transaction has been failed.</h2>
              {
                !!error.message
                  ? !!error.details
                    ? <div><p>{error.message}</p><p>{error.details}</p></div>
                    : <p>{error.message}</p>
                  : <p>error</p>
              }
            </div>
        }
        {
          !!txHash &&
            <div>
              <h2>Transaction has been submited.</h2>
              <p>Transaction Hash: {txHash}</p>
              {
                !refId
                  ? <p style={{ color: 'red' }}>Reference Id: 'Waitig for transaction..'</p>
                  : <p>Reference Id: {refId}</p>
              }
              <p><a className="form-link" href={`https://preview.cardanoscan.io/transaction/${txHash}`}>Open transaction in explorer</a></p>
            </div>
        }
      </Modal>
    </div>
  );
}

export default FormScriptDeploy;