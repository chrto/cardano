import { useState } from 'react';
import './AccordionForm.css';
import AccordionItem from './AccordionItem';
import FormScriptCreate from './FormScriptCreate';

export default function AccordionScriptsForm( ) {
  const [openIndex, setOpenIndex] = useState(null);

  const toggle = (index) => {
    setOpenIndex(openIndex === index ? null : index);
  };

  return (
    <form className="accordion-form">
      <AccordionItem title="Add New" isOpen={openIndex === 0} onToggle={() => toggle(0)}>
        <FormScriptCreate  />
      </AccordionItem>
    </form>
  );
}
