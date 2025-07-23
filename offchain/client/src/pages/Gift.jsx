import './Page.css'
import { useEffect, useState, useRef } from 'react';
import Wallet from '../components/Wallet';
import getData from '../utils/getDataFromServer';
import Navbar from '../components/Navbar';
import dispatchData from '../utils/dispatchData';
import useSafeInterval from '../utils/useSafeInterval';
import AccordionGiftForm from '../components/AccordionGiftForm';
import AccordionGiftView from '../components/AccordionGiftView';
import AccordionWalletView from '../components/AccordionWalletView';
import getKeyUTxO from '../utils/getKeyUTxO';

const { giftScript, apiRefreshDelay } = require("../config.json");

function Gift({ publicKeyHash, walletAddress, walletUtxos }) {
  const walletViewRef = useRef(null);
  const scriptViewRef = useRef(null);

  const [scriptAddress, setScriptAddress] = useState([]);
  const [scriptUtxos, setScriptUtxos] = useState([]);
  const [selectedScript, setSelectedScript] = useState({name: "giftScript", script: giftScript});
  const [enableInterval, setEnableInterval] = useState(false)

  useEffect(() => {
    setState()
      .then(_ => setEnableInterval(true))
  }, []);

  useSafeInterval(async () => setState(), enableInterval ? apiRefreshDelay : null);

  const setState = async () =>
    Promise.resolve(getScritptByName("giftScript"))
      .then(dispatchData(setSelectedScript))
      .then(selected => selected.script)
      .then(getScriptAddress)
      .then(dispatchData(setScriptAddress))
      .then(getAddressUtxos)
      .then(dispatchData(setScriptUtxos))
      .catch((err) => console.error('Fetch error:', err));

  const getScritptByName = (name) => ({
    name, script: giftScript
  });

  const getAddressUtxos = async address =>
    getData(`cardano/${address}/utxos`)
      .catch(e => {
        console.error(`Can not fetch utxos for address ${address} from server!\n origin: ${e.message}`)
        return [];
      })

  const getScriptAddress = async script =>
    getData(`cardano/script/address?type=${script.type}&script=${script.script}`)
      .then(({ address }) => address)
      .catch(e => {
        console.error(`Can not fetch script address for script ${script.script} from server!\n origin: ${e.message}`)
        return "...";
      })

  const getSelectedWalletUtxos = () => {
    const selected = walletViewRef.current?.getSelected();
    return walletUtxos.filter(utxo => selected.has(getKeyUTxO(utxo)))
  }

  const deselectWalletUtxos = () => {
    walletViewRef.current?.deselect();
  }

  const getSelectedScriptUtxos = () => {
    const selected = scriptViewRef.current?.getSelected();
    return scriptUtxos.filter(utxo => selected.has(getKeyUTxO(utxo)))
  }

  const deselectScriptUtxos = () => {
    scriptViewRef.current?.deselect();
  }

  return (
    <div className="app-container">
      <Navbar />
      <div className="wallet-content">
        <Wallet publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos  ={walletUtxos} />
        <AccordionWalletView walletUtxos={walletUtxos} ref={walletViewRef} />
      </div>
      <div className="main-content">
        <aside className="sidebar">
          <AccordionGiftForm
            scriptAddress={scriptAddress}
            validatorScript={giftScript}
            getSelectedWalletUtxos={getSelectedWalletUtxos}
            deselectWalletUtxos={deselectWalletUtxos}
            getSelectedScriptUtxos={getSelectedScriptUtxos}
            deselectScriptUtxos={deselectScriptUtxos}
          />
        </aside>
        <div className="view-panel">
          <AccordionGiftView
            scriptUtxos={scriptUtxos}
            scriptAddress={scriptAddress}
            selectedScript={selectedScript}
            ref={scriptViewRef}
          />
        </div>
      </div>
    </div>
  );
}

export default Gift;
