import { useState } from 'react';
import './AccordionForm.css';
import AccordionItem from './AccordionItem';
import FormVestingSend from './FormVestingSend';
import FormVestingParametrizedSend from './FormVestingParametrizedSend';
import FormVestingClaim from './FormVestingClaim';
import ChoiceScriptVersion from './ChoiceScriptVersion';

export default function AccordionVestingForm({ scriptAddress, handleChoice, selected, scripts, publicKeyHash, getSelectedWalletUtxos, deselectWalletUtxos, getSelectedScriptUtxos, getReferenceUtxo, deselect} ) {
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
        {scriptAddress === "addr_test1wp53ycwlfuuylz6ttxcwury3p9ejfsu5tpq3m8d57l7ad8cqpk94v"
          ? <FormVestingSend scriptAddress={scriptAddress} getSelectedWalletUtxos={getSelectedWalletUtxos} deselectWalletUtxos={deselectWalletUtxos} />
          : <FormVestingParametrizedSend scriptAddress={scriptAddress} getSelectedWalletUtxos={getSelectedWalletUtxos} deselectWalletUtxos={deselectWalletUtxos} />
        }
      </AccordionItem>

      <AccordionItem title="Claim" isOpen={openIndex === 1} onToggle={() => toggle(1)}>
        <FormVestingClaim publicKeyHash={publicKeyHash} validatorScript={ validatorScript } getSelectedScriptUtxos={getSelectedScriptUtxos} getReferenceUtxo={getReferenceUtxo} deselect={deselect} />
      </AccordionItem>
    </form>
  );
}
