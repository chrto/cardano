import { useState } from 'react';
import AccordionItem from './AccordionItem';
import './AccordionForm.css';
import FormGiftSend from './FormGiftSend';
import FormGiftCollect from './FormGiftCollect';

export default function AccordionGiftForm({ scriptAddress, walletUtxos, scriptUtxos, validatorScript } ) {
  const [openIndex, setOpenIndex] = useState(null);

  const toggle = (index) => {
    setOpenIndex(openIndex === index ? null : index);
  };

  return (
    <form className="accordion-form">
      <AccordionItem title="Send Gift" isOpen={openIndex === 0} onToggle={() => toggle(0)}>
        <FormGiftSend scriptAddress={scriptAddress} walletUtxos={walletUtxos} />
      </AccordionItem>

      <AccordionItem title="Collect Gift" isOpen={openIndex === 1} onToggle={() => toggle(1)}>
        <FormGiftCollect scriptUtxos={scriptUtxos} validatorScript={ validatorScript } />
      </AccordionItem>
    </form>
  );
}
