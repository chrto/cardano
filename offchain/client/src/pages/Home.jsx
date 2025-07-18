import './Page.css'
import Navbar from '../components/Navbar';
import Header from '../components/Header';
import AccordionWalletView from '../components/AccordionWalletView';

export default function Home({publicKeyHash, walletAddress, walletUtxos}) {
  return (
    <div className="app-container">
      <Navbar />
      <Header publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos  ={walletUtxos} />
      <AccordionWalletView walletUtxos={walletUtxos} />

      <div className="main-content">
        <h1>Home Page</h1>
      </div>
    </div>
  );
}