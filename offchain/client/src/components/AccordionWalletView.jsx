import './AccordionForm.css';
import { useState } from 'react';
import AccordionItem from './AccordionItem';
import Table from './Table'

export default function AccordionWalletView({ walletUtxos } ) {
  const [openWallet, setOpenWallet] = useState(false);

  const convertWalletUtxos = utxo => [`${utxo.txId}#${utxo.txIndex}`, utxo.assets.lovelace, Number(utxo.assets.lovelace) / 1000000]

  return (
    <form className="accordion-form">
      <AccordionItem title={"Wallet Utxo's (" + walletUtxos.map(convertWalletUtxos).length + ")"} isOpen={openWallet} onToggle={() => setOpenWallet(!openWallet)}>
        <Table
          headers={['UTXO', 'Value [Lovelace]', 'Value [Ada]']}
          values={walletUtxos.map(convertWalletUtxos)}
        />
      </AccordionItem>
    </form>
  );
}
