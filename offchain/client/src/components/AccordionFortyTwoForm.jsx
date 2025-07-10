import { useState } from 'react';
import AccordionItem from './AccordionItem';
import './AccordionForm.css';
import FormFortyTwoSend from './FormFortyTwoSend';
import FormFortyTwoCollect from './FormFortyTwoCollect';
import ChoiceFortyTwo from '../components/ChoiceFortyTwo';

export default function AccordionFortyTwoForm({ scriptAddress, walletUtxos, scriptUtxos, validatorScript, handleChoice, selected} ) {
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
        <FormFortyTwoSend scriptAddress={scriptAddress} walletUtxos={walletUtxos} />
      </AccordionItem>

      <AccordionItem title="Collect FortyTwo" isOpen={openIndex === 1} onToggle={() => toggle(1)}>
        <FormFortyTwoCollect scriptUtxos={scriptUtxos} validatorScript={ validatorScript } />
      </AccordionItem>
    </form>
  );
}
