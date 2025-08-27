import { useState } from 'react';
import AccordionItem from './AccordionItem';
import './AccordionForm.css';
import FormGiftSend from './FormGiftSend';
import FormGiftCollect from './FormGiftCollect';
import ChoiceScriptVersion from './ChoiceScriptVersion';

export default function AccordionGiftForm({ scriptAddress, handleChoice, selected, scripts, getSelectedWalletUtxos, deselectWalletUtxos, getSelectedScriptUtxos, getReferenceUtxo, deselect } ) {
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
      <AccordionItem title="Send Gift" isOpen={openIndex === 0} onToggle={() => toggle(0)}>
        <FormGiftSend scriptAddress={scriptAddress} getSelectedWalletUtxos={getSelectedWalletUtxos} deselectWalletUtxos={deselectWalletUtxos} />
      </AccordionItem>

      <AccordionItem title="Collect Gift" isOpen={openIndex === 1} onToggle={() => toggle(1)}>
        <FormGiftCollect validatorScript={validatorScript} getSelectedScriptUtxos={getSelectedScriptUtxos} getReferenceUtxo={getReferenceUtxo} deselect={deselect} />
      </AccordionItem>
    </form>
  );
}
