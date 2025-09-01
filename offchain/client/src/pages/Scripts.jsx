import './Page.css'
import { useState, useEffect, useRef } from 'react';
import Navbar from '../components/Navbar';
import Wallet from '../components/Wallet';
import AccordionWalletView from '../components/AccordionWalletView';
import AccordionWalletForm from '../components/AccordionWalletForm';
import AccordionScriptsView from '../components/AccordionScriptsView';
import useSafeInterval from '../utils/useSafeInterval';
import getData from '../utils/getDataFromServer';
import dispatchData from '../utils/dispatchData';
import AccordionScriptsForm from '../components/AccordionScriptsForm';
import getKeyUTxO from '../utils/getKeyUTxO';

const { apiRefreshDelay } = require("../config.json");

function Scripts({ publicKeyHash, walletAddress, walletUtxos }) {
  const walletViewRef = useRef(null);
  const scriptViewRef = useRef(null);

  const [scripts, setScripts] = useState([]);
  const [enableInterval, setEnableInterval] = useState(false)

  useEffect(() => {
    setState()
      .then(_ => setEnableInterval(true))
  }, []);

  useSafeInterval(async () => setState(), enableInterval ? apiRefreshDelay : null);

  const setState = async () =>
    getScripts()
      .then(dispatchData(setScripts))
      .then(scripts => scripts[0])
      .catch((err) => console.error('Fetch error:', err));

  const getScripts = async () =>
    getData(`cardano/scripts`)
      .catch(e => {
        console.error(`Can not fetch 'FortyTwo' script from server!\n origin: ${e.message}`)
        return [];
      })

  const getSelectedScript = () => {
    const selected = scriptViewRef.current?.getSelected();
    return scripts.find(script => selected === script.id)
  }

  const deselectScript = () => {
    scriptViewRef.current?.deselect();
  }

  const getSelectedWalletUtxos = () => {
    const selected = walletViewRef.current?.getSelected();
    return walletUtxos.filter(utxo => selected.has(getKeyUTxO(utxo)))
  }

  const deselectWalletUtxos = () => {
    walletViewRef.current?.deselect();
  }

  return (
    <div className="app-container">
      <Navbar />
      <div className="partial-content">
        <Wallet publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos={walletUtxos} />
        <AccordionWalletView walletUtxos={walletUtxos} ref={walletViewRef} />
        <AccordionWalletForm walletUtxos={walletUtxos} />
      </div>
      <div className="partial-content">
        <AccordionScriptsForm
          getSelectedScript={getSelectedScript}
          deselectScript={deselectScript}
          getSelectedWalletUtxos={getSelectedWalletUtxos}
          deselectWalletUtxos={deselectWalletUtxos}
        />
      </div>
      <div className="partial-content" >
        <AccordionScriptsView
          scripts={scripts}
          ref={scriptViewRef}
        />
      </div>
    </div>
  );
}

export default Scripts;
