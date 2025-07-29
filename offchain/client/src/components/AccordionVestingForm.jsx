import { useState } from 'react';
import './AccordionForm.css';
import AccordionItem from './AccordionItem';
import FormVestingSend from './FormVestingSend';
import FormVestingClaim from './FormVestingClaim';
import ChoiceVesting from './ChoiceVesting';

export default function AccordionVestingForm({ scriptAddress, validatorScript, handleChoice, selected, publicKeyHash, getSelectedWalletUtxos, deselectWalletUtxos, getSelectedScriptUtxos, deselectScriptUtxos} ) {
  const [openIndex, setOpenIndex] = useState(null);
  const [openSelectScript, setOpenSelectScript] = useState(true);

  const toggle = (index) => {
    setOpenIndex(openIndex === index ? null : index);
  };

  return (
    <form className="accordion-form">
      <AccordionItem title="Select Script Version" isOpen={openSelectScript} onToggle={() => setOpenSelectScript(!openSelectScript)}>
        <ChoiceVesting handleChoice={handleChoice} selected={selected}/>
      </AccordionItem>

      <AccordionItem title="Vest" isOpen={openIndex === 0} onToggle={() => toggle(0)}>
        <FormVestingSend scriptAddress={scriptAddress} getSelectedWalletUtxos={getSelectedWalletUtxos} deselectWalletUtxos={deselectWalletUtxos} />
      </AccordionItem>

      <AccordionItem title="Claim" isOpen={openIndex === 1} onToggle={() => toggle(1)}>
        <FormVestingClaim publicKeyHash={publicKeyHash} validatorScript={ validatorScript } getSelectedScriptUtxos={getSelectedScriptUtxos} deselectScriptUtxos={deselectScriptUtxos} />
      </AccordionItem>
    </form>
  );
}
