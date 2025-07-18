import { useState } from 'react';
import AccordionItem from './AccordionItem';
import './AccordionForm.css';
import Table from './Table'
import ScriptDetail from './ScriptDetail'
import { datumFromCBOR } from '../utils/lucid/data';


export default function AccordionVestingView({ publicKeyHash, scriptUtxos, scriptAddress, selectedScript } ) {
  const [openIndex, setOpenIndex] = useState(0);
  const [openScriptDetail, setOpenScriptDetail] = useState(true);

  const convertMineVestingUTxOs = utxo => [
    `${utxo.txId}#${utxo.txIndex}`,
    getDeadline(utxo),
    Number(utxo.assets.lovelace) / 1000000
  ]

  const convertOthersVestingUTxOs = utxo => [
    `${utxo.txId}#${utxo.txIndex}`,
    getBeneficiary(utxo),
    getDeadline(utxo),
    Number(utxo.assets.lovelace) / 1000000
  ]

  const filterMineVest = utxo => datumFromCBOR(utxo.datum, 'vesting').beneficiary === publicKeyHash
  const filterOtherVest = utxo => !filterMineVest(utxo)

  const getDeadline = utxo => new Date(Number(datumFromCBOR(utxo.datum, 'vesting').deadline)).toISOString()
  const getBeneficiary = utxo => datumFromCBOR(utxo.datum, 'vesting').beneficiary

  const toggle = (index) => {
    setOpenIndex(openIndex === index ? null : index);
  };

  return (
    <form className="accordion-form">
      <AccordionItem title={"Script"} isOpen={openScriptDetail} onToggle={() => setOpenScriptDetail(!openScriptDetail)}>
        <ScriptDetail scriptAddress={scriptAddress} selectedScript={selectedScript} />
      </AccordionItem>

      <AccordionItem title={"Script Utxo's with Me as beneficiary (" + scriptUtxos.filter(filterMineVest).length + ")"} isOpen={openIndex === 0} onToggle={() => toggle(0)}>
        <Table
          headers={['UTXO', 'Deadline', 'Value [Ada]']}
          values={scriptUtxos.filter(filterMineVest).map(convertMineVestingUTxOs)}
        />
      </AccordionItem>
      <AccordionItem title={"Script Utxo's with other as beneficiaries (" + scriptUtxos.filter(filterOtherVest).length + ")"} isOpen={openIndex === 1} onToggle={() => toggle(1)}>
        <Table
          headers={['UTXO', 'Beneficiary', 'Deadline', 'Value [Ada]']}
          values={scriptUtxos.filter(filterOtherVest).map(convertOthersVestingUTxOs)}
        />
      </AccordionItem>
    </form>
  );
}
