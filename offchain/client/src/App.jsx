import React, { useEffect, useState } from 'react';
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import Home from './pages/Home';
import About from './pages/About';
import Gift from './pages/Gift';
import FortyTwo from './pages/FortyTwo';
import Vesting from './pages/Vesting';
import getData from './utils/getDataFromServer';
import dispatchData from './utils/dispatchData';
import lucidStorage from './utils/lucid/storage';
import useSafeInterval from './utils/useSafeInterval';

const {  apiRefreshDelay } = require('./config.json');

function App() {
  const [publicKeyHash, setPublicKeyHash] = useState("...");
  const [walletUtxos, setWalletUtxos] = useState([]);
  const [walletAddress, setWalletAddress] = useState("...");
  const [enableInterval, setEnableInterval] = useState(false)

  useEffect(() => {
    setState()
      .then(_ => setEnableInterval(true))
  }, []);

  useSafeInterval(async () => setState(), enableInterval ? apiRefreshDelay : null);

  const setState = async () =>
    getWalletAddress()
      .then(dispatchData(setWalletAddress))
      .then(address => {
        Promise.all([
          getCardanoPKH(address)
            .then(dispatchData(setPublicKeyHash))
            .catch((err) => console.error('Fetch error:', err)),
          getAddressUtxos(address)
            .then(dispatchData(setWalletUtxos))
            .catch((err) => console.error('Fetch error:', err))
        ])
      })

  const getWalletAddress = async () => lucidStorage.then(storage => storage.getWalletAddress())
  const getCardanoPKH = async address =>
    getData(`cardano/${address}/credential/payment`)
      .then(({ hash }) => hash)
      .catch(e => {
        console.error(`Can not fetch pubKeyHash for address ${address} from server!\n origin: ${e.message}`)
        return "...";
      })

  const getAddressUtxos = async address =>
    getData(`cardano/${address}/utxos`)
      .catch(e => {
        console.error(`Can not fetch utxos for address ${address} from server!\n origin: ${e.message}`)
        return [];
      })

  return (
    <Router>
      <Routes>
        <Route path="/" element={<Home publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos={walletUtxos} />} />
        <Route path="/about" element={<About publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos={walletUtxos} />} />
        <Route path="/gift" element={<Gift publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos={walletUtxos} />} />
        <Route path="/fortyTwo" element={<FortyTwo publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos={walletUtxos} />} />
        <Route path="/vesting" element={<Vesting publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos={walletUtxos} />} />
      </Routes>
    </Router>
  );
}

export default App;