import React, { useEffect, useState } from 'react';
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import Home from './pages/Home';
import About from './pages/About';
import Gift from './pages/Gift';
import FortyTwo from './pages/FortyTwo';

import getData from './utils/getDataFromServer';
import dispatchData from './utils/dispatchData';

import lucidStorage from './utils/lucid/storage';

function App() {
  const [publicKeyHash, setPublicKeyHash] = useState("...");
  const [walletUtxos, setWalletUtxos] = useState([]);
  const [walletAddress, setWalletAddress] = useState("...");

  useEffect(() => {
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
  }, []);

  const getWalletAddress = async () => lucidStorage.then(storage => storage.getWalletAddress())
  const getCardanoPKH = async address =>
    getData(`cardano/${address}/credential/payment`)
      .then(({ hash }) => hash)
  const getAddressUtxos = async address =>
    getData(`cardano/${address}/utxos`)
      .then(utxos => {
        console.debug(`There are ${utxos.length} UTxOs at address ${address}`)
        return utxos
      })

  return (
    <Router>
      <Routes>
        <Route path="/" element={<Home publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos={walletUtxos} />} />
        <Route path="/about" element={<About publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos={walletUtxos} />} />
        <Route path="/gift" element={<Gift publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos={walletUtxos} />} />
        <Route path="/fortyTwo" element={<FortyTwo publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos={walletUtxos} />} />
      </Routes>
    </Router>
  );
}

export default App;