import './Page.css'
import Navbar from '../components/Navbar';
import Wallet from '../components/Wallet';
import AccordionWalletView from '../components/AccordionWalletView';

export default function About({publicKeyHash, walletAddress, walletUtxos}) {
  return (
    <div className="app-container">
      <Navbar />
      <div className="partial-content">
        <Wallet publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos  ={walletUtxos} />
        <AccordionWalletView walletUtxos={walletUtxos} />
      </div>

      <div className="partial-content">
        <h1>About Page</h1>
      </div>
    </div>
  );
}