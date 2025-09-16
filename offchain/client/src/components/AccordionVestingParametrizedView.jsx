import { useState, useImperativeHandle, useEffect } from 'react';
import AccordionItem from './AccordionItem';
import './AccordionForm.css';
import Table from './Table'
import ScriptDetail from './ScriptDetail'
import { datumFromCBOR } from '../utils/lucid/data';
import getKeyUTxO from '../utils/getKeyUTxO';
import dispatchData from '../utils/dispatchData';
import getData from '../utils/getDataFromServer';

export default function AccordionVestingParametrizedView({ scriptUtxos, scriptAddress, selectedScript, ref }) {
  const [selectedUtxos, setSelectedUtxos] = useState(new Set([]));
  const [selectedRef, setSelectedRef] = useState(null);
  const [openUtxoIndex, setOpenUtxoIndex] = useState(0);
  const [openScriptIndex, setOpenScriptIndex] = useState(0);
  const [utxos, setUtxos] = useState({ reference: [], wrongDatum: [], beforeDeadline: [], afterDeadline: [] });
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

  const sortUtxos = (availableUtxos) => availableUtxos.reduce((acc, utxo) => {
    const deadline = getDeadline(utxo);
    return {
      ...acc,
      ...!!utxo.scriptHash
        ? { reference: [...acc.reference, utxo] }
        : !!deadline
          ? Date.now() > deadline
            ? { afterDeadline: [...acc.afterDeadline, utxo] }
            : { beforeDeadline: [...acc.beforeDeadline, utxo] }
          : { wrongDatum: [...acc.wrongDatum, utxo] }
    }
  }, { reference: [], wrongDatum: [], beforeDeadline: [], afterDeadline: [] })

  const getDeadline = utxo => {
    const datum = datumFromCBOR(utxo.datum, 'vestingParametrized')
    return !!datum
      ? Number(datum)
      : null
  }
  const getDeadlineISO = utxo => new Date(getDeadline(utxo)).toISOString()

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

  const convertUtxos = utxo => ({
    key: getKeyUTxO(utxo),
    link: true,
    select: false,
    data: [utxo.txId, utxo.txIndex, utxo.assets.lovelace, Number(utxo.assets.lovelace) / 1000000]
  })

  const convertReferences = ref => ({
    key: getKeyUTxO(ref),
    link: true,
    select: true,
    data: [isSelectedRef(ref), ref.txId, ref.txIndex, ref.address]
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
        <AccordionItem title={"Vesting Script"} isOpen={openScriptIndex === 0} onToggle={() => toggleScript(0)}>
          <ScriptDetail scriptAddress={scriptAddress} selectedScript={selectedScript} />
        </AccordionItem>

        <AccordionItem title={"Script referenc Utxo's (" + references.length + ")"} isOpen={openScriptIndex === 1} onToggle={() => toggleScript(1)}>
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
        <AccordionItem title={"Script Utxo's After Deadline (" + utxos.afterDeadline.length + ")"} isOpen={openUtxoIndex === 0} onToggle={() => toggleUtxo(0)}>
          <Table
            headers={['', 'TxHash', 'TxIdx', 'Deadline', 'Value [Ada]']}
            values={utxos.afterDeadline.map(convertMineVestingUTxOs)}
            selectRow={selectUtxo}
          />
        </AccordionItem>
        <AccordionItem title={"Script Utxo's Before Deadline (" + utxos.beforeDeadline.length + ")"} isOpen={openUtxoIndex === 1} onToggle={() => toggleUtxo(1)}>
          <Table
            headers={['', 'TxHash', 'TxIdx', 'Deadline', 'Value [Ada]']}
            values={utxos.beforeDeadline.map(convertMineVestingUTxOs)}
            selectRow={selectUtxo}
          />
        </AccordionItem>
        <AccordionItem title={"Script Utxo's with wrong datum - Can not be spend (" + utxos.wrongDatum.length + ")"} isOpen={openUtxoIndex === 1} onToggle={() => toggleUtxo(1)}>
          <Table
            headers={['TxHash', 'TxIdx', 'Value [Lovelace]', 'Value [Ada]']}
            values={utxos.wrongDatum.map(convertUtxos)}
            selectRow={selectUtxo}
          />
        </AccordionItem>
      </div>
    </form>
  );
}
