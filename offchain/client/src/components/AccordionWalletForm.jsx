import './AccordionForm.css';
import { useState } from 'react';
import AccordionItem from './AccordionItem';
import FormAdaSend from './FormAdaSend';

export default function AccordionWalletForm({ walletUtxos, ref }) {
  const [openWalletSend, setOpenWalletSend] = useState(false);

  return (
    <form className="accordion-form" >
      <AccordionItem title={"Send"} isOpen={openWalletSend} onToggle={() => setOpenWalletSend(!openWalletSend)}>
        <FormAdaSend />
      </AccordionItem>
    </form>
  );
}
