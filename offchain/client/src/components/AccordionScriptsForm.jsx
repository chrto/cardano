import { useState } from 'react';
import './AccordionForm.css';
import AccordionItem from './AccordionItem';
import FormScriptCreate from './FormScriptCreate';
import FormScriptDeploy from './FormScriptDeploy';

export default function AccordionScriptsForm({ getSelectedScript, deselectScript}) {
  const [openIndex, setOpenIndex] = useState(null);

  const toggle = (index) => {
    setOpenIndex(openIndex === index ? null : index);
  };

  return (
    <form className="accordion-form">
      <AccordionItem title="Add Script" isOpen={openIndex === 0} onToggle={() => toggle(0)}>
        <FormScriptCreate  />
      </AccordionItem>
      <AccordionItem title="Deploy Script" isOpen={openIndex === 1} onToggle={() => toggle(1)}>
        <FormScriptDeploy
          getSelectedScript={getSelectedScript}
          deselectScript={deselectScript}
        />
      </AccordionItem>
    </form>
  );
}
