import './Page.css'
import { useEffect, useState, useRef } from 'react';
import AccordionFortyTwoView from '../components/AccordionFortyTwoView';
import Wallet from '../components/Wallet';
import AccordionFortyTwoForm from '../components/AccordionFortyTwoForm'
import AccordionWalletView from '../components/AccordionWalletView';
import getData from '../utils/getDataFromServer';
import Navbar from '../components/Navbar';
import dispatchData from '../utils/dispatchData';
import useSafeInterval from '../utils/useSafeInterval';
import getKeyUTxO from '../utils/getKeyUTxO';

const { apiRefreshDelay } = require("../config.json");

function FortyTwo({ publicKeyHash, walletAddress, walletUtxos }) {
  const walletViewRef = useRef(null);
  const scriptViewRef = useRef(null);

  const [scriptAddress, setScriptAddress] = useState("...");
  const [scripts, setScripts] = useState([]);
  const [scriptUtxos, setScriptUtxos] = useState([]);
  const [selectedScript, setSelectedScript] = useState(null);
  const [enableInterval, setEnableInterval] = useState(false)

  useEffect(() => {
    setState()
      .then(_ => setEnableInterval(true))
  }, []);

  useSafeInterval(async () => refresh(), enableInterval ? apiRefreshDelay : null);

  const refresh = async () =>
    getScripts()
      .then(dispatchData(setScripts))
      .then(_ => getAddressUtxos(scriptAddress))
      .then(dispatchData(setScriptUtxos))
      .catch((err) => console.error('Fetch error:', err));

  const setState = async () =>
    getScripts()
      .then(dispatchData(setScripts))
      .then(scripts => scripts[0])
      .then(dispatchData(setSelectedScript))
      .then(getScriptAddress)
      .then(dispatchData(setScriptAddress))
      .then(getAddressUtxos)
      .then(dispatchData(setScriptUtxos))
      .catch((err) => console.error('Fetch error:', err));

  const handleChoice = (e) => {
    dispatchData(setScriptUtxos)([])
    Promise.resolve(getScritptById(e.target.value))
      .then(dispatchData(setSelectedScript))
      .then(getScriptAddress)
      .then(dispatchData(setScriptAddress))
      .then(getAddressUtxos)
      .then(dispatchData(setScriptUtxos))
      .catch((err) => console.error('Fetch error:', err))
  };

  const getScritptById = (id) => scripts.find(script => script.id === id);

  const getScripts = async () =>
    getData(`cardano/scripts?category=FortyTwo`)
      .catch(e => {
        console.error(`Can not fetch 'FortyTwo' script from server!\n origin: ${e.message}`)
        return [];
      })

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
        <Wallet publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos={walletUtxos} />
        <AccordionWalletView walletUtxos={walletUtxos} ref={walletViewRef} />
      </div>
      {selectedScript &&
        <div className="main-content">
          <aside className="sidebar">
            <AccordionFortyTwoForm
              scriptAddress={scriptAddress}
              handleChoice={handleChoice}
              selected={selectedScript}
              scripts={scripts}
              getSelectedWalletUtxos={getSelectedWalletUtxos}
              deselectWalletUtxos={deselectWalletUtxos}
              getSelectedScriptUtxos={getSelectedScriptUtxos}
              deselectScriptUtxos={deselectScriptUtxos}
            />
          </aside>
          <div className="view-panel">
            <AccordionFortyTwoView
              scriptUtxos={scriptUtxos}
              scriptAddress={scriptAddress}
              selectedScript={selectedScript}
              ref={scriptViewRef}
            />
          </div>
        </div>
      }
    </div>
  );
}

export default FortyTwo;
