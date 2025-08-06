import { useState, useImperativeHandle } from 'react';
import AccordionItem from './AccordionItem';
import './AccordionForm.css';
import Table from './Table'
import ScriptDetail from './ScriptDetail'
import convertToJs from '../utils/convertToJs'
import { Data } from 'lucid-cardano'
import getKeyUTxO from '../utils/getKeyUTxO';

export default function AccordionFortyTwoView({ scriptUtxos, scriptAddress, selectedScript, ref }) {
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

  const convertUtxosCheckBox = utxo => ({
    key: getKeyUTxO(utxo),
    link: true,
    select: true,
    data: [isSelectedUTxO(utxo), utxo.txId, utxo.txIndex, utxo.assets.lovelace, Number(utxo.assets.lovelace) / 1000000]
  })
  const convertUtxos = utxo => ({
    key: getKeyUTxO(utxo),
    link: true,
    select: false,
    data: [utxo.txId, utxo.txIndex, utxo.assets.lovelace, Number(utxo.assets.lovelace) / 1000000]
  })
  const convertScriptUtxosInlineDatum = utxo => ({
    key: getKeyUTxO(utxo),
    link: true,
    select: false,
    data: [utxo.txId, utxo.txIndex, utxo.datum, JSON.stringify(convertToJs(Data.from(utxo.datum)), null, 2), Number(utxo.assets.lovelace) / 1000000]
  })
  const convertScriptUtxosHashDatum = utxo => ({
    key: getKeyUTxO(utxo),
    link: true,
    select: false,
    data: [utxo.txId, utxo.txIndex, utxo.datumHash, utxo.assets.lovelace, Number(utxo.assets.lovelace) / 1000000]
  })

  const filterInlineDatumFortyTwo = utxo => utxo.datum === 'd87980'
  const filterInlineDatum = utxo => !!utxo.datum && !filterInlineDatumFortyTwo(utxo)
  const filterHashDatum = utxo => !!utxo.datumHash
  const filterNoDatum = utxo => !utxo.datumHash && !utxo.datum

  const isSelectedUTxO = (utxo) => selectedUtxos.has(getKeyUTxO(utxo))

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
      <div className="partial-content">
        <AccordionItem title={"Script"} isOpen={openScriptDetail} onToggle={() => setOpenScriptDetail(!openScriptDetail)}>
          <ScriptDetail scriptAddress={scriptAddress} selectedScript={selectedScript} />
        </AccordionItem>
      </div>
      <div className="partial-content" >
        <AccordionItem title={"Script Utxo's with Unit InLine Datum (" + scriptUtxos.filter(filterInlineDatumFortyTwo).length + ")"} isOpen={openIndex === 0} onToggle={() => toggle(0)}>
          <Table
            headers={['', 'TxHash', 'TxIdx', 'Value [Lovelace]', 'Value [Ada]']}
            values={scriptUtxos.filter(filterInlineDatumFortyTwo).map(convertUtxosCheckBox).sort((x, y) => y[4] - x[4])}
            selectRow={selectUtxo}
          />
        </AccordionItem>
        <AccordionItem title={"Script Utxo's without Datum (" + scriptUtxos.filter(filterNoDatum).length + ")"} isOpen={openIndex === 1} onToggle={() => toggle(1)}>
          <Table
            headers={['TxHash', 'TxIdx', 'Value [Lovelace]', 'Value [Ada]']}
            values={scriptUtxos.filter(filterNoDatum).map(convertUtxos)}
          />
        </AccordionItem>
        <AccordionItem title={"Script Utxo's with InLine Datum (" + scriptUtxos.filter(filterInlineDatum).length + ")"} isOpen={openIndex === 2} onToggle={() => toggle(2)}>
          <Table
            headers={['TxHash', 'TxIdx', 'Datum', 'Datum Decoded', 'Value [Ada]']}
            values={scriptUtxos.filter(filterInlineDatum).map(convertScriptUtxosInlineDatum)}
          />
        </AccordionItem>
        <AccordionItem title={"Script Utxo's with Hash Datum (" + scriptUtxos.filter(filterHashDatum).length + ")"} isOpen={openIndex === 3} onToggle={() => toggle(3)}>
          <Table
            headers={['TxHash', 'TxIdx', 'Datum Hash', 'Value [Lovelace]', 'Value [Ada]']}
            values={scriptUtxos.filter(filterHashDatum).map(convertScriptUtxosHashDatum)}
          />
        </AccordionItem>
      </div>
    </form>
  );
}
