import { useState, useImperativeHandle } from 'react';
import AccordionItem from './AccordionItem';
import './AccordionForm.css';
import Table from './Table'
import ScriptDetail from './ScriptDetail'
import convertToJs from '../utils/convertToJs'
import { Data } from 'lucid-cardano'
import getKeyUTxO from '../utils/getKeyUTxO';

export default function AccordionGiftView({ scriptUtxos, scriptAddress, selectedScript, ref }) {

  const [selectedUtxos, setSelectedUtxos] = useState(new Set([]));
  const [openUtxoIndex, setOpenUtxoIndex] = useState(0);
  const [openScriptIndex, setOpenScriptIndex] = useState(0);

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

  const filterInlineDatumUnit = utxo => utxo.datum === 'd87980'
  const filterInlineDatum = utxo => !!utxo.datum && !filterInlineDatumUnit(utxo)
  const filterHashDatum = utxo => !!utxo.datumHash
  const filterNoDatum = utxo => !utxo.datumHash && !utxo.datum

  const toggleUtxo = (index) => {
    setOpenUtxoIndex(openUtxoIndex === index ? null : index);
  };

  const toggleScript = (index) => {
    setOpenScriptIndex(openScriptIndex === index ? null : index);
  };

  const isSelectedUTxO = (utxo) => selectedUtxos.has(getKeyUTxO(utxo))

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
        <AccordionItem title={"Gift Script"} isOpen={openScriptIndex === 0} onToggle={() => toggleScript(0)}>
          <ScriptDetail scriptAddress={scriptAddress} selectedScript={selectedScript} />
        </AccordionItem>

        <AccordionItem title={"Script Utxo's sitting on this address (" + [].length + ")"} isOpen={openScriptIndex === 1} onToggle={() => toggleScript(1)}>
          <Table
            headers={['TxHash', 'TxIdx', 'category', 'Title']}
            values={[]}
          />
        </AccordionItem>

        <AccordionItem title={"Script Utxo's Gift (" + [].length + ")"} isOpen={openScriptIndex === 2} onToggle={() => toggleScript(2)}>
          <Table
            headers={['TxHash', 'TxIdx', 'Title']}
            values={[]}
          />
        </AccordionItem>
      </div>
      <div className="partial-content" >
        <AccordionItem title={"Script Utxo's with Unit InLine Datum (" + scriptUtxos.filter(filterInlineDatumUnit).length + ")"} isOpen={openUtxoIndex === 0} onToggle={() => toggleUtxo(0)}>
          <Table
            headers={['', 'TxHash', 'TxIdx', 'Value [Lovelace]', 'Value [Ada]']}
            values={scriptUtxos.filter(filterInlineDatumUnit).map(convertUtxosCheckBox).sort((x, y) => y[4] - x[4])}
            selectRow={selectUtxo}
          />
        </AccordionItem>
        <AccordionItem title={"Script Utxo's without Datum (" + scriptUtxos.filter(filterNoDatum).length + ")"} isOpen={openUtxoIndex === 1} onToggle={() => toggleUtxo(1)}>
          <Table
            headers={['TxHash', 'TxIdx', 'Value [Lovelace]', 'Value [Ada]']}
            values={scriptUtxos.filter(filterNoDatum).map(convertUtxos)}
          />
        </AccordionItem>
        <AccordionItem title={"Script Utxo's with InLine Datum (" + scriptUtxos.filter(filterInlineDatum).length + ")"} isOpen={openUtxoIndex === 2} onToggle={() => toggleUtxo(2)}>
          <Table
            headers={['TxHash', 'TxIdx', 'Datum', 'Datum Decoded', 'Value [Ada]']}
            values={scriptUtxos.filter(filterInlineDatum).map(convertScriptUtxosInlineDatum)}
          />
        </AccordionItem>
        <AccordionItem title={"Script Utxo's with Hash Datum (" + scriptUtxos.filter(filterHashDatum).length + ")"} isOpen={openUtxoIndex === 3} onToggle={() => toggleUtxo(3)}>
          <Table
            headers={['TxHash', 'TxIdx', 'Datum Hash', 'Value [Lovelace]', 'Value [Ada]']}
            values={scriptUtxos.filter(filterHashDatum).map(convertScriptUtxosHashDatum)}
          />
        </AccordionItem>
      </div>
    </form>
  );
}
