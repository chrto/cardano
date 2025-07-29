import './Page.css'
import { useEffect, useState, useRef } from 'react';
import AccordionVestingView from '../components/AccordionVestingView';
import Wallet from '../components/Wallet';
import AccordionVestingForm from '../components/AccordionVestingForm'
import AccordionWalletView from '../components/AccordionWalletView';
import getData from '../utils/getDataFromServer';
import Navbar from '../components/Navbar';
import dispatchData from '../utils/dispatchData';
import useSafeInterval from '../utils/useSafeInterval';
import getKeyUTxO from '../utils/getKeyUTxO';

const {vestingScript, apiRefreshDelay} = require("../config.json");

function Vesting({ publicKeyHash, walletAddress, walletUtxos }) {
  const walletViewRef = useRef(null);
  const scriptViewRef = useRef(null);

  const [scriptAddress, setScriptAddress] = useState("...");
  const [scriptUtxos, setScriptUtxos] = useState([]);
  const [selectedScript, setSelectedScript] = useState({ name: "vestingScript", script: vestingScript });
  const [enableInterval, setEnableInterval] = useState(false)

  useEffect(() => {
    setState()
      .then(_ => setEnableInterval(true))
  }, []);

  useSafeInterval(async () => setState(), enableInterval ? apiRefreshDelay : null);

  const setState = async () =>
    getScriptAddress(selectedScript.script)
      .then(dispatchData(setScriptAddress))
      .then(getAddressUtxos)
      .then(dispatchData(setScriptUtxos))
      .catch((err) => console.error('Fetch error:', err))

  const getScritptByName = (name) => {
    switch (name) {
      case 'vestingScript': return {name: "vestingScript", script: vestingScript};
      default: return {name: "vestingScript", script: vestingScript};
    }
  }

  const handleChoice = (e) => {
    dispatchData(setScriptUtxos)([])
    Promise.resolve(getScritptByName(e.target.value))
      .then(dispatchData(setSelectedScript))
      .then(selected => selected.script)
      .then(getScriptAddress)
      .then(dispatchData(setScriptAddress))
      .then(getAddressUtxos)
      .then(dispatchData(setScriptUtxos))
      .catch((err) => console.error('Fetch error:', err))
  };

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
          <AccordionVestingForm
            scriptAddress={scriptAddress}
            validatorScript={selectedScript.script}
            handleChoice={handleChoice}
            selected={selectedScript.name}
            publicKeyHash={publicKeyHash}
            getSelectedWalletUtxos={getSelectedWalletUtxos}
            deselectWalletUtxos={deselectWalletUtxos}
            getSelectedScriptUtxos={getSelectedScriptUtxos}
            deselectScriptUtxos={deselectScriptUtxos}
          />
        </aside>
        <div className="view-panel">
          <AccordionVestingView
            publicKeyHash={publicKeyHash}
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

export default Vesting;
