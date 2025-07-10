import React, {  } from 'react';
import './Choice.css';

function ChoiceFortyTwo({ handleChoice, selected }) {
  return (
    <div className="choice">
      <div className='choiceItem'>
        <input type="radio" id="scriptChoice1" name="contact" value="fortyTwoScript" onChange={handleChoice} checked={selected==="fortyTwoScript"} />
        <label htmlFor="scriptChoice1">FortyTwo Untyped</label>
      </div>
      <div className='choiceItem'>
        <input type="radio" id="scriptChoice2" name="contact" value="fortyTwoTypedScript" onChange={handleChoice} checked={selected==="fortyTwoTypedScript"} />
        <label htmlFor="scriptChoice2">FortyTwo Typed</label>
      </div>
      <div className='choiceItem'>
        <input type="radio" id="scriptChoice3" name="contact" value="fortyTwoTypedScriptP" onChange={handleChoice} checked={selected==="fortyTwoTypedScriptP"} />
        <label htmlFor="scriptChoice3">FortyTwo Typed Mine</label>
      </div>
     </div>
  );
}

export default ChoiceFortyTwo;