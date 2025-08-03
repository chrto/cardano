import './AccordionForm.css';
import { useState, useImperativeHandle } from 'react';
import AccordionItem from './AccordionItem';
import Table from './Table'
import getKeyUTxO from '../utils/getKeyUTxO';

export default function AccordionWalletView({ walletUtxos, ref }) {
  const [selectedUtxos, setSelectedUtxos] = useState(new Set([]));
  const [openWallet, setOpenWallet] = useState(false);

  // Expose functions to parent
  useImperativeHandle(ref, () => ({
    getSelected() {
      return selectedUtxos
    },
    deselect() {
      setSelectedUtxos(new Set([]))
    }
  }));

  const convertWalletUtxos = utxo => ({
    key: getKeyUTxO(utxo),
    link: true,
    select: true,
    data: [isSelectedUTxO(utxo), utxo.txId, utxo.txIndex, utxo.assets.lovelace, Number(utxo.assets.lovelace) / 1000000]
  })

  const selectUtxo = (utxoKey) => {
    const selection = new Set(selectedUtxos)
    selection.has(utxoKey)
      ? selection.delete(utxoKey)
      : selection.add(utxoKey)

    setSelectedUtxos(selection)
  }

  const isSelectedUTxO = (utxo) => selectedUtxos.has(getKeyUTxO(utxo))

  return (
    <form className="accordion-form" >
      <AccordionItem title={"Wallet Utxo's (" + walletUtxos.map(convertWalletUtxos).length + ")"} isOpen={openWallet} onToggle={() => setOpenWallet(!openWallet)}>
        <Table
          headers={['', 'TxHash', 'TxIdx', 'Value [Lovelace]', 'Value [Ada]']}
          values={walletUtxos.map(convertWalletUtxos)}
          selectUtxo={selectUtxo}
        />
      </AccordionItem>
    </form>
  );
}
