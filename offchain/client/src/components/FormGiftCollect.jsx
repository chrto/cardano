import './Form.css';
import utxoToLucid from '../utils/utxoToLucid';
import lucidStorage from '../utils/lucid/storage';

function FormGiftCollect({ validatorScript, getSelectedScriptUtxos, deselectScriptUtxos}) {

  const handleSubmit = (e) => {
    e.preventDefault();

    const utxos = getSelectedScriptUtxos().map(utxoToLucid)
    if (utxos.length === 0) {
      return alert("No UTxO has been selected!");
    }

    lucidStorage.then(storage =>
      storage.buildSpendFromContractTx(validatorScript, utxos)
        .then(storage.signTx)
        .then(storage.submitTx)
        .then(storage.successHandler)
        .then(() => {
          resetForms()
        })
        .catch(storage.errorHandler)
    )
  };

  const resetForms = () => {
    deselectScriptUtxos()
  }

  const handleReset = (e) => {
    e.preventDefault();
    resetForms()
  }

  return (
    <div className="form">
      <button type="button" onClick={handleSubmit}>Submit</button>
      <button type="button" onClick={handleReset}>Reset</button>
    </div>
  );
}

export default FormGiftCollect;