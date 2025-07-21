import './Page.css'
import Navbar from '../components/Navbar';
import Wallet from '../components/Wallet';
import AccordionWalletView from '../components/AccordionWalletView';

export default function Home({publicKeyHash, walletAddress, walletUtxos}) {
  return (
    <div className="app-container">
      <Navbar />
      <div className="wallet-content">
        <Wallet publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos  ={walletUtxos} />
        <AccordionWalletView walletUtxos={walletUtxos} />
      </div>

      <div className="main-content">
        <h1>Home Page</h1>
      </div>
    </div>
  );
}