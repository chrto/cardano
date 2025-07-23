import { useState } from 'react';
import './Form.css';

function FormAdaSend() {
  const [formData, setFormData] = useState({ amount: 3 });

  const handleChange = (e) => {
    e.preventDefault();

    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const handleSubmit = (e) => {
    e.preventDefault();

    alert("Not implemented yet..")
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
    </div>
  );
}

export default FormAdaSend;