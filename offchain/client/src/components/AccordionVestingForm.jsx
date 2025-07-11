import { useState } from 'react';
import './AccordionForm.css';
import AccordionItem from './AccordionItem';
import FormVestingSend from './FormVestingSend';
import FormFortyTwoCollect from './FormFortyTwoCollect';
import ChoiceVesting from './ChoiceVesting';

export default function AccordionVestingForm({ scriptAddress, walletUtxos, scriptUtxos, validatorScript, handleChoice, selected} ) {
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
        <FormVestingSend scriptAddress={scriptAddress} walletUtxos={walletUtxos} />
      </AccordionItem>

      <AccordionItem title="Claim" isOpen={openIndex === 1} onToggle={() => toggle(1)}>
        <FormFortyTwoCollect scriptUtxos={scriptUtxos} validatorScript={ validatorScript } />
      </AccordionItem>
    </form>
  );
}
