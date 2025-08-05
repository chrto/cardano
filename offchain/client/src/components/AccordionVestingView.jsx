import { useState, useImperativeHandle } from 'react';
import AccordionItem from './AccordionItem';
import './AccordionForm.css';
import Table from './Table'
import ScriptDetail from './ScriptDetail'
import { datumFromCBOR } from '../utils/lucid/data';
import getKeyUTxO from '../utils/getKeyUTxO';

export default function AccordionVestingView({ publicKeyHash, scriptUtxos, scriptAddress, selectedScript, ref }) {
  const [selectedUtxos, setSelectedUtxos] = useState(new Set([]));

  const [openIndex, setOpenIndex] = useState(0);
  const [openScriptDetail, setOpenScriptDetail] = useState(true);

  // Expose functions to parent
  useImperativeHandle(ref, () => ({
    getSelected() {
      return selectedUtxos
    },
    deselect() {
      setSelectedUtxos(new Set([]))
    }
  }));

  const isSelectedUTxO = (utxo) => selectedUtxos.has(getKeyUTxO(utxo))

  const convertMineVestingUTxOs = utxo => ({
    key: getKeyUTxO(utxo),
    link: true,
    select: true,
    data: [
      isSelectedUTxO(utxo),
      utxo.txId,
      utxo.txIndex,
      getDeadlineISO(utxo),
      Number(utxo.assets.lovelace) / 1000000
    ]
  })

  const convertOthersVestingUTxOs = utxo => ({
    key: getKeyUTxO(utxo),
    link: true,
    select: false,
    data: [
      utxo.txId,
      utxo.txIndex,
      getBeneficiary(utxo),
      getDeadlineISO(utxo),
      Number(utxo.assets.lovelace) / 1000000
    ]
  })

  const getBeneficiary = utxo => datumFromCBOR(utxo.datum, 'vesting')?.beneficiary

  const getDeadline = utxo => {
    const datum = datumFromCBOR(utxo.datum, 'vesting')
    return !!datum
      ? Number(datum.deadline)
      : null
  }

  const filterMineVest = utxo => getBeneficiary(utxo) === publicKeyHash
  const filterOtherVest = utxo => !filterMineVest(utxo)
  const afterDeadline = utxo => filterMineVest(utxo) && Date.now() > getDeadline(utxo)
  const beforeDeadline = utxo => filterMineVest(utxo) && Date.now() <= getDeadline(utxo)

  const getDeadlineISO = utxo => new Date(getDeadline(utxo)).toISOString()

  const toggle = (index) => {
    setOpenIndex(openIndex === index ? null : index);
  };

  const selectUtxo = (utxoKey) => {
    const selection = new Set(selectedUtxos)
    selection.has(utxoKey)
      ? selection.delete(utxoKey)
      : selection.add(utxoKey)

    setSelectedUtxos(selection)
  }

  return (
    <form className="accordion-form">
      <AccordionItem title={"Script"} isOpen={openScriptDetail} onToggle={() => setOpenScriptDetail(!openScriptDetail)}>
        <ScriptDetail scriptAddress={scriptAddress} selectedScript={selectedScript} />
      </AccordionItem>

      <AccordionItem title={"Script Utxo's After Deadline (" + scriptUtxos.filter(afterDeadline).length + ")"} isOpen={openIndex === 0} onToggle={() => toggle(0)}>
        <Table
          headers={['', 'TxHash', 'TxIdx', 'Deadline', 'Value [Ada]']}
          values={scriptUtxos.filter(afterDeadline).map(convertMineVestingUTxOs)}
          selectRow={selectUtxo}
        />
      </AccordionItem>
      <AccordionItem title={"Script Utxo's Before Deadline (" + scriptUtxos.filter(beforeDeadline).length + ")"} isOpen={openIndex === 1} onToggle={() => toggle(1)}>
        <Table
          headers={['', 'TxHash', 'TxIdx', 'Deadline', 'Value [Ada]']}
          values={scriptUtxos.filter(beforeDeadline).map(convertMineVestingUTxOs)}
          selectRow={selectUtxo}
        />
      </AccordionItem>
      <AccordionItem title={"Script Utxo's with other as beneficiaries (" + scriptUtxos.filter(filterOtherVest).length + ")"} isOpen={openIndex === 2} onToggle={() => toggle(2)}>
        <Table
          headers={['TxHash', 'TxIdx', 'Beneficiary', 'Deadline', 'Value [Ada]']}
          values={scriptUtxos.filter(filterOtherVest).map(convertOthersVestingUTxOs)}
        />
      </AccordionItem>
    </form>
  );
}
