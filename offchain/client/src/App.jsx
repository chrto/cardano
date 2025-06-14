import React, { useEffect, useState } from 'react';
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import Home from './pages/Home';
import About from './pages/About';
import Gift from './pages/Gift';

import getData from './utils/getDataFromServer';
import dispatchData from './utils/dispatchData';

import { Lucid, Blockfrost, Kupmios } from 'lucid-cardano';

const { provider, wallet } = require("./config.json");

function App() {
  const [lucid, setLucid] = useState([]);
  const [publicKeyHash, setPublicKeyHash] = useState([]);
  const [walletUtxos, setWalletUtxos] = useState([]);
  const [walletAddress, setWalletAddress] = useState([]);

  useEffect(() => {
    loadCardano()
      .then(dispatchData(setLucid))
      .then(getWalletAddress)
      .then(address => {
        setWalletAddress(address)
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

  const getPorvider =
    provider.use === "blockFrost"
      ? new Blockfrost(provider[provider.use].url, provider[provider.use].projectId)
      : provider.use === "node"
        ? new Kupmios(provider[provider.use].kupo, provider[provider.use].ogmios)
          : null

  const loadCardano = async () =>
    Lucid.new(getPorvider, provider.network)
      .then(setWallet)

  //  "privateKey" | "api" | ...
  const setWallet = async (lucid) =>
    wallet.use === "privateKey"
      ? Promise.resolve(lucid.selectWalletFromPrivateKey(wallet[wallet.use]))
        .then(_ => lucid)
      : window.cardano[wallet[wallet.use]].enable()
        .then(api => lucid.selectWallet(api))
        .then(_ => lucid)

  const getWalletAddress = async lucid => lucid.wallet.address()
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
        <Route path="/gift" element={<Gift lucid={lucid} publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos={walletUtxos} />} />
      </Routes>
    </Router>
  );
}

export default App;