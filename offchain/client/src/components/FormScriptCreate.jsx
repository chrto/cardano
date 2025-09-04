import React, { useState } from 'react';
import './Form.css';
import Modal from './Modal';
import postDataToServer from '../utils/postDataToServer';
import dispatchData from '../utils/dispatchData';

function FormScriptCreate() {
  const [formData, setFormData] = useState({ type: 'PlutusV2', title: '', script: '', category: 'Gift', description: '' });
  const [error, setError] = useState(null);
  const [result, setResult] = useState(null);

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    postDataToServer(`cardano/scripts`, formData)
      .then(dispatchData(setResult))
      .catch(dispatchData(setError))
  };

  const handleReset = (e) => {
    e.preventDefault();
    setFormData({ ...formData, type: 'PlutusV2', title: '', script: '', category: 'Gift', description: '' });
  }

  const handleCloseModal = (e) => {
    e.preventDefault();

    setError(null)
    setResult(null)
  }

  return (
    <div className="form">
      <div className="inputs">
        <label>Script Type:</label>
        <select name="type" id="type" onChange={handleChange}>
          <option value="PlutusV1">PlutusV1</option>
          <option value="PlutusV2">PlutusV2</option>
          <option value="PlutusV3">PlutusV3</option>
          <option value="Native">Native</option>
        </select>

        <label>Title:</label>
        <input type="text" name="title" value={formData.title} onChange={handleChange} />

        <label>Script CBOR:</label>
        <input type="text" name="script" value={formData.script} onChange={handleChange} />

        <label>Category:</label>
        <select name="category" id="category" onChange={handleChange}>
          <option value="Gift">Gift</option>
          <option value="Burn">Burn</option>
          <option value="FortyTwo">FortyTwo</option>
          <option value="Vesting">Vesting</option>
        </select>

        <label>Description:</label>
        <input type="text" name="description" value={formData.description} onChange={handleChange} />
      </div>

      <div className="buttons">
        <button type="button" onClick={handleSubmit}>Submit</button>
        <button type="button" onClick={handleReset}>Reset</button>
      </div>

      <Modal isOpen={!!result || !!error} isError={!!error} onClose={handleCloseModal}>
        {
          !!result &&
            <div>
              <h2>Script has been added into DB.</h2>
              <p>{result.type}</p>
              <p>{result.category}</p>
              <p>{result.title}</p>
              <p>{result.description}</p>
            </div>
        }
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
      </Modal>
    </div>
  );
}

export default FormScriptCreate;