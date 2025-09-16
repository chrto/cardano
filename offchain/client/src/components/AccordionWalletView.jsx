import './AccordionForm.css';
import { useState, useImperativeHandle, useEffect } from 'react';
import AccordionItem from './AccordionItem';
import Table from './Table'
import getKeyUTxO from '../utils/getKeyUTxO';
import dispatchData from '../utils/dispatchData';

export default function AccordionWalletView({ walletUtxos, ref }) {
  const [selectedUtxos, setSelectedUtxos] = useState(new Set([]));
  const [openUtxoIndex, setOpenUtxoIndex] = useState(100);
  const [utxos, setUtxos] = useState({ reference: [], others: [] });

  // Expose functions to parent
  useImperativeHandle(ref, () => ({
    getSelected() {
      return selectedUtxos
    },
    deselect() {
      setSelectedUtxos(new Set([]))
    }
  }));

  useEffect(() => {
    if (walletUtxos && walletUtxos.length > 0) {
      Promise.resolve(walletUtxos)
        .then(sortUtxos)
        .then(dispatchData(setUtxos))
    }
  }, [walletUtxos]);

  const sortUtxos = (availableUtxos) => availableUtxos.reduce((acc, utxo) => ({
    ...acc,
    ...!!utxo.scriptHash
        ? { reference: [...acc.reference, utxo] }
        : { others: [...acc.others, utxo] }
  }), { reference: [], others: [] })

  const convertWalletUtxos = utxo => ({
    key: getKeyUTxO(utxo),
    link: true,
    select: true,
    data: [isSelectedUTxO(utxo), utxo.txId, utxo.txIndex, utxo.assets.lovelace, Number(utxo.assets.lovelace) / 1000000]
  })

  const convertDeploymentUtxos = utxo => ({
    key: getKeyUTxO(utxo),
    link: true,
    select: false,
    data: [utxo.txId, utxo.txIndex, utxo.assets.lovelace, Number(utxo.assets.lovelace) / 1000000]
  })

  const selectUtxo = (utxoKey) => {
    const selection = new Set(selectedUtxos)
    selection.has(utxoKey)
      ? selection.delete(utxoKey)
      : selection.add(utxoKey)

    setSelectedUtxos(selection)
  }

  const isSelectedUTxO = (utxo) => selectedUtxos.has(getKeyUTxO(utxo))

  const toggleUtxo = (index) => {
    setOpenUtxoIndex(openUtxoIndex === index ? null : index);
  };

  return (
    <form className="accordion-form" >
      <AccordionItem title={"Wallet Utxo's (" + utxos.others.length + ")"} isOpen={openUtxoIndex === 0} onToggle={() => toggleUtxo(0)}>
        <Table
          headers={['', 'TxHash', 'TxIdx', 'Value [Lovelace]', 'Value [Ada]']}
          values={utxos.others.map(convertWalletUtxos)}
          selectRow={selectUtxo}
        />
      </AccordionItem>

      <AccordionItem title={"Deployed Scripts (" + utxos.reference.length + ")"} isOpen={openUtxoIndex === 1} onToggle={() => toggleUtxo(1)}>
        <Table
          headers={['TxHash', 'TxIdx', 'Value [Lovelace]', 'Value [Ada]']}
          values={utxos.reference.map(convertDeploymentUtxos)}
          selectRow={selectUtxo}
        />
      </AccordionItem>
    </form>
  );
}
