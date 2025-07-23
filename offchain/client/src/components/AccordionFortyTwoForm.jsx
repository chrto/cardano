import { useState } from 'react';
import './AccordionForm.css';
import AccordionItem from './AccordionItem';
import FormFortyTwoSend from './FormFortyTwoSend';
import FormFortyTwoCollect from './FormFortyTwoCollect';
import ChoiceFortyTwo from './ChoiceFortyTwo';

export default function AccordionFortyTwoForm({ scriptAddress, validatorScript, handleChoice, selected, getSelectedWalletUtxos, deselectWalletUtxos, getSelectedScriptUtxos, deselectScriptUtxos } ) {
  const [openIndex, setOpenIndex] = useState(null);
  const [openSelectScript, setOpenSelectScript] = useState(true);

  const toggle = (index) => {
    setOpenIndex(openIndex === index ? null : index);
  };

  return (
    <form className="accordion-form">
      <AccordionItem title="Select Script Version" isOpen={openSelectScript} onToggle={() => setOpenSelectScript(!openSelectScript)}>
        <ChoiceFortyTwo handleChoice={handleChoice} selected={selected}/>
      </AccordionItem>

      <AccordionItem title="Send FortyTwo" isOpen={openIndex === 0} onToggle={() => toggle(0)}>
        <FormFortyTwoSend scriptAddress={scriptAddress} getSelectedWalletUtxos={getSelectedWalletUtxos} deselectWalletUtxos={deselectWalletUtxos} />
      </AccordionItem>

      <AccordionItem title="Collect FortyTwo" isOpen={openIndex === 1} onToggle={() => toggle(1)}>
        <FormFortyTwoCollect validatorScript={validatorScript} getSelectedScriptUtxos={getSelectedScriptUtxos} deselectScriptUtxos={deselectScriptUtxos} />
      </AccordionItem>
    </form>
  );
}
