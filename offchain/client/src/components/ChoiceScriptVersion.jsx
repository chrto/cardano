import './Choice.css';

function ChoiceScriptVersion({ handleChoice, selected, scripts }) {
  return (
    <div className="choice">
      {scripts.map((script) => (
        <div className='choiceItem' key={script.id}>
          <input type="radio" id={script.id} name={script.category} value={script.id} onChange={handleChoice} checked={selected===script.id} />
          <label htmlFor={script.id}>{ script.title }</label>
        </div>))
      }
     </div>
  );
}

export default ChoiceScriptVersion;