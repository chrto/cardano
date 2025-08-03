import { useState } from 'react';
import AccordionItem from './AccordionItem';
import './AccordionForm.css';
import Table from './Table'

export default function AccordionScriptsView({ scripts }) {
  const [openIndex, setOpenIndex] = useState(0);

  const toggle = (index) => {
    setOpenIndex(openIndex === index ? null : index);
  };

  const filterBurn = script => script.category === 'Burn';
  const filterGift = script => script.category === 'Gift';
  const filterFortyTwo = script => script.category === 'FortyTwo';
  const filterVesting = script => script.category === 'Vesting';

  const formToLine = script => ([
    script.title,
    script.type,
    script.description
  ]);

  return (
    <form className="accordion-form">
      <AccordionItem title={"Burn (" + scripts.filter(filterBurn).length + ")"} isOpen={openIndex === 0} onToggle={() => toggle(0)}>
        <Table
          headers={['Title', 'Type', 'Description']}
          values={scripts.filter(filterBurn).map(formToLine)}
        />
      </AccordionItem>

      <AccordionItem title={"Gift (" + scripts.filter(filterGift).length + ")"} isOpen={openIndex === 1} onToggle={() => toggle(1)}>
        <Table
          headers={['Title', 'Type', 'Description']}
          values={scripts.filter(filterGift).map(formToLine)}
        />
      </AccordionItem>

      <AccordionItem title={"FortyTwo (" + scripts.filter(filterFortyTwo).length + ")"} isOpen={openIndex === 2} onToggle={() => toggle(2)}>
        <Table
          headers={['Title', 'Type', 'Description']}
          values={scripts.filter(filterFortyTwo).map(formToLine)}
        />
      </AccordionItem>

      <AccordionItem title={"Vesting (" + scripts.filter(filterVesting).length + ")"} isOpen={openIndex === 3} onToggle={() => toggle(3)}>
        <Table
          headers={['Title', 'Type', 'Description']}
          values={scripts.filter(filterVesting).map(formToLine)}
        />
      </AccordionItem>

    </form>
  );
}
