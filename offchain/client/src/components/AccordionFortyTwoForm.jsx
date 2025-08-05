import { useState } from 'react';
import './AccordionForm.css';
import AccordionItem from './AccordionItem';
import FormFortyTwoSend from './FormFortyTwoSend';
import FormFortyTwoCollect from './FormFortyTwoCollect';
import ChoiceScriptVersion from './ChoiceScriptVersion';

export default function AccordionFortyTwoForm({ scriptAddress, handleChoice, selected, scripts, getSelectedWalletUtxos, deselectWalletUtxos, getSelectedScriptUtxos, deselectScriptUtxos } ) {
  const [openIndex, setOpenIndex] = useState(null);
  const [openSelectScript, setOpenSelectScript] = useState(true);

  const toggle = (index) => {
    setOpenIndex(openIndex === index ? null : index);
  };

  const validatorScript = {
    script: selected.script,
    type: selected.type
  }

  return (
    <form className="accordion-form">
      {scripts.length > 1 &&
        <AccordionItem title="Select Script Version" isOpen={openSelectScript} onToggle={() => setOpenSelectScript(!openSelectScript)}>
          <ChoiceScriptVersion handleChoice={handleChoice} selected={selected.id} scripts={ scripts } />
        </AccordionItem>
      }

      <AccordionItem title="Send FortyTwo" isOpen={openIndex === 0} onToggle={() => toggle(0)}>
        <FormFortyTwoSend scriptAddress={scriptAddress} getSelectedWalletUtxos={getSelectedWalletUtxos} deselectWalletUtxos={deselectWalletUtxos} />
      </AccordionItem>

      <AccordionItem title="Collect FortyTwo" isOpen={openIndex === 1} onToggle={() => toggle(1)}>
        <FormFortyTwoCollect validatorScript={validatorScript} getSelectedScriptUtxos={getSelectedScriptUtxos} deselectScriptUtxos={deselectScriptUtxos} />
      </AccordionItem>
    </form>
  );
}
