import { useState, useImperativeHandle, useEffect } from 'react';
import AccordionItem from './AccordionItem';
import './AccordionForm.css';
import Table from './Table'
import ScriptDetail from './ScriptDetail'
import convertToJs from '../utils/convertToJs'
import { Data } from 'lucid-cardano'
import getKeyUTxO from '../utils/getKeyUTxO';
import dispatchData from '../utils/dispatchData';
import getData from '../utils/getDataFromServer';

export default function AccordionFortyTwoView({ scriptUtxos, scriptAddress, selectedScript, ref }) {
  const [selectedUtxos, setSelectedUtxos] = useState(new Set([]));
  const [selectedRef, setSelectedRef] = useState(null);
  const [openUtxoIndex, setOpenUtxoIndex] = useState(0);
  const [openScriptIndex, setOpenScriptIndex] = useState(0);
  const [utxos, setUtxos] = useState({ reference: [], inlineUnit: [], inline: [], hash: [], noDatum: [] });
  const [references, setReferences] = useState([]);

  // Expose functions to parent
  useImperativeHandle(ref, () => ({
    getSelected() {
      return selectedUtxos
    },
    getReference() {
      return selectedRef
    },
    deselect() {
      setSelectedUtxos(new Set([]))
      setSelectedRef(null)
    }
  }));

  useEffect(() => {
    if (scriptUtxos) {
      Promise.all([
        Promise.resolve(scriptUtxos)
          .then(sortUtxos)
          .then(dispatchData(setUtxos)),
        getreferences()
          .then(dispatchData(setReferences))
      ])
    }
  }, [scriptUtxos]);

  const getreferences = () =>
    getData(`cardano/scriptReferences?scriptId=${selectedScript.id}`)
      .then(refs => refs.map(ref => ({ txId: ref.txId, txIndex: ref.txIndex, address: ref.address })));

  const sortUtxos = (availableUtxos) => availableUtxos.reduce((acc, utxo) => ({
    ...acc,
    ...!!utxo.scriptHash
      ? { reference: [...acc.reference, utxo] }
      : !!utxo.datum
        ? (utxo.datum === 'd87980'
          ? { inlineUnit: [...acc.inlineUnit, utxo] }
          : { inline: [...acc.inline, utxo] })
        : !!utxo.datumHash
          ? { hash: [...acc.hash, utxo] }
          : { noDatum: [...acc.noDatum, utxo] }
  }), { reference: [], inlineUnit: [], inline: [], hash: [], noDatum: [] })

  const convertUtxosCheckBox = utxo => ({
    key: getKeyUTxO(utxo),
    link: true,
    select: true,
    data: [isSelectedUTxO(utxo), utxo.txId, utxo.txIndex, utxo.assets.lovelace, Number(utxo.assets.lovelace) / 1000000]
  })

  const convertReferences = ref => ({
    key: getKeyUTxO(ref),
    link: true,
    select: true,
    data: [isSelectedRef(ref), ref.txId, ref.txIndex, ref.address]
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

  const isSelectedRef = (ref) => getKeyUTxO(ref) === selectedRef
  const selectRef = (refKey) => setSelectedRef(refKey === selectedRef ? null : refKey)

  return (
    <form className="accordion-form">
      <div className="partial-content">
        <AccordionItem title={"FortyTwo Script"} isOpen={openScriptIndex === 0} onToggle={() => toggleScript(0)}>
          <ScriptDetail scriptAddress={scriptAddress} selectedScript={selectedScript} />
        </AccordionItem>

        <AccordionItem title={"Script referenc Utxo's - FortyTwo (" + references.length + ")"} isOpen={openScriptIndex === 1} onToggle={() => toggleScript(1)}>
          <Table
            headers={['', 'TxHash', 'TxIdx', 'Address']}
            values={references.map(convertReferences)}
            selectRow={selectRef}
          />
        </AccordionItem>
        <AccordionItem title={"Script reference Utxo's sitting on this address (" + utxos.reference.length + ")"} isOpen={openScriptIndex === 2} onToggle={() => toggleScript(2)}>
          <Table
            headers={['TxHash', 'TxIdx', 'Value [Lovelace]', 'Value [Ada]']}
            values={utxos.reference.map(convertUtxos)}
          />
        </AccordionItem>
      </div>

      <div className="partial-content" >
        <AccordionItem title={"Script Utxo's with Unit InLine Datum (" + utxos.inlineUnit.length + ")"} isOpen={openUtxoIndex === 0} onToggle={() => toggleUtxo(0)}>
          <Table
            headers={['', 'TxHash', 'TxIdx', 'Value [Lovelace]', 'Value [Ada]']}
            values={utxos.inlineUnit.map(convertUtxosCheckBox).sort((x, y) => y[4] - x[4])}
            selectRow={selectUtxo}
          />
        </AccordionItem>
        <AccordionItem title={"Script Utxo's without Datum (" + utxos.noDatum.length + ")"} isOpen={openUtxoIndex === 1} onToggle={() => toggleUtxo(1)}>
          <Table
            headers={['TxHash', 'TxIdx', 'Value [Lovelace]', 'Value [Ada]']}
            values={utxos.noDatum.map(convertUtxos)}
          />
        </AccordionItem>
        <AccordionItem title={"Script Utxo's with InLine Datum (" + utxos.inline.length + ")"} isOpen={openUtxoIndex === 2} onToggle={() => toggleUtxo(2)}>
          <Table
            headers={['TxHash', 'TxIdx', 'Datum', 'Datum Decoded', 'Value [Ada]']}
            values={utxos.inline.map(convertScriptUtxosInlineDatum)}
          />
        </AccordionItem>
        <AccordionItem title={"Script Utxo's with Hash Datum (" + utxos.hash.length + ")"} isOpen={openUtxoIndex === 3} onToggle={() => toggleUtxo(3)}>
          <Table
            headers={['TxHash', 'TxIdx', 'Datum Hash', 'Value [Lovelace]', 'Value [Ada]']}
            values={utxos.hash.map(convertScriptUtxosHashDatum)}
          />
        </AccordionItem>
      </div>
    </form>
  );
}
