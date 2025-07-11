import './Choice.css';

function ChoiceVesting({ handleChoice, selected }) {
  return (
    <div className="choice">
      <div className='choiceItem'>
        <input type="radio" id="scriptChoice1" name="vesting" value="vestingScript" onChange={handleChoice} checked={selected==="vestingScript"} />
        <label htmlFor="scriptChoice1">Vesting</label>
      </div>
    </div>
  );
}

export default ChoiceVesting;