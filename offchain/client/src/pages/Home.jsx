import './Page.css'
import Navbar from '../components/Navbar';
import Wallet from '../components/Wallet';
import AccordionWalletView from '../components/AccordionWalletView';
import AccordionWalletForm from '../components/AccordionWalletForm';

export default function Home({ publicKeyHash, walletAddress, walletUtxos }) {
  return (
    <div className="app-container">
      <Navbar />
      <div className="partial-content">
        <Wallet publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos  ={walletUtxos} />
        <AccordionWalletView walletUtxos={walletUtxos} />
        <AccordionWalletForm walletUtxos={walletUtxos} />
      </div>

      <div className="partial-content">
        <h1>Home Page</h1>
      </div>
    </div>
  );
}