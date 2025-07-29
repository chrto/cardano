import { useState } from 'react';
import './Form.css';
import Modal from './Modal';

function FormAdaSend() {
  const [formData, setFormData] = useState({ amount: 3 });
  const [isModalOpen, setModalOpen] = useState(false);

  const handleChange = (e) => {
    e.preventDefault();

    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    setModalOpen(true)
  };

  return (
    <div className="form">
      <div className="inputs">
        <label>Amount in ADA:</label>
        <input type="number" name="amount" value={formData.amount} onChange={handleChange} />
      </div>
      <div className='buttons'>
        <button type="button" onClick={handleSubmit}>Submit</button>
      </div>

      <Modal isOpen={isModalOpen} onClose={() => setModalOpen(false)}>
        <h2>Not implemented yet.</h2>
        <p>Working on this feature..</p>
      </Modal>
    </div>
  );
}

export default FormAdaSend;