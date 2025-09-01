import { useState } from 'react';
import './Form.css';
import utxoToLucid from '../utils/utxoToLucid';
import lucidStorage from '../utils/lucid/storage';
import Modal from './Modal';
import dispatchData from '../utils/dispatchData';

function FormFortyTwoSend({ scriptAddress, getSelectedWalletUtxos, deselectWalletUtxos }) {
  const [formData, setFormData] = useState({ valueAda: 3 });
  const [error, setError] = useState(null);
  const [txHash, setTxHash] = useState(null);

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();
    // eslint-disable-next-line no-undef
    const amountLovelace = BigInt(formData.valueAda) * BigInt(1000000)
    const utxos = getSelectedWalletUtxos().map(utxoToLucid)

    const options = {
      amountLovelace,
      contractAddress: scriptAddress,
      datum: null
    }
    lucidStorage.then(storage =>
      storage.buildPayToContractTx(utxos, options)
        .then(storage.signTx)
        .then(storage.submitTx)
        .then(dispatchData(setTxHash))
        .catch(dispatchData(setError))
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

  const handleCloseModal = (e) => {
    e.preventDefault();

    setError(null)
    setTxHash(null)
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

export default FormFortyTwoSend;