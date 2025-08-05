import { useState } from 'react';
import './AccordionForm.css';
import AccordionItem from './AccordionItem';
import FormVestingSend from './FormVestingSend';
import FormVestingClaim from './FormVestingClaim';
import ChoiceScriptVersion from './ChoiceScriptVersion';

export default function AccordionVestingForm({ scriptAddress, handleChoice, selected, scripts, publicKeyHash, getSelectedWalletUtxos, deselectWalletUtxos, getSelectedScriptUtxos, deselectScriptUtxos} ) {
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

      <AccordionItem title="Vest" isOpen={openIndex === 0} onToggle={() => toggle(0)}>
        <FormVestingSend scriptAddress={scriptAddress} getSelectedWalletUtxos={getSelectedWalletUtxos} deselectWalletUtxos={deselectWalletUtxos} />
      </AccordionItem>

      <AccordionItem title="Claim" isOpen={openIndex === 1} onToggle={() => toggle(1)}>
        <FormVestingClaim publicKeyHash={publicKeyHash} validatorScript={ validatorScript } getSelectedScriptUtxos={getSelectedScriptUtxos} deselectScriptUtxos={deselectScriptUtxos} />
      </AccordionItem>
    </form>
  );
}
