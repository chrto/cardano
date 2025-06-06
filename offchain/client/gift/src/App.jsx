import React, { useEffect, useState } from 'react';
import './App.css';
import Views from './components/Views';
import Header from './components/Header';
import FormSend from './components/FormSend';
import FormCollect from './components/FormCollect';
import { Lucid, Blockfrost, Kupmios } from 'lucid-cardano';
import getData from './utils/getDataFromServer';

const { giftScript, provider, wallet } = require("./config.json");

function App() {
  const [lucid, setLucid] = useState([]);
  const [publicKeyHash, setPublicKeyHash] = useState([]);
  const [balance, setBalance] = useState([]);
  const [walletUtxos, setWalletUtxos] = useState([]);
  const [scriptUtxos, setScriptUtxos] = useState([]);
  const [scriptAddress, setScriptAddress] = useState([]);
  const [walletAddress, setWalletAddress] = useState([]);

  useEffect(() => {
    Promise.all([
      loadCardano()
        .then(dispatch(setLucid))
        .then(getWalletAddress)
        .then(address => {
          setWalletAddress(address)
          Promise.all([
            getCardanoPKH(address)
              .then(dispatch(setPublicKeyHash))
              .catch((err) => console.error('Fetch error:', err)),
            getAddressUtxos(address)
              .then(dispatch(setWalletUtxos))
              .then(getBalanceLovelace)
              .then(getBalanceAda)
              .then(dispatch(setBalance))
              .catch((err) => console.error('Fetch error:', err))
          ])
        }),
      getScriptAddress(giftScript)
        .then(dispatch(setScriptAddress))
        .then(getAddressUtxos)
        .then(dispatch(setScriptUtxos))
        .catch((err) => console.error('Fetch error:', err))
    ])
  }, []);

  const dispatch = (dispatcher) => (value) => {
    dispatcher(value)
    return value
  }

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

  const getBalanceLovelace = utxos => utxos.reduce((acc, utxo) => acc + utxo.assets.lovelace, 0n)
  const getBalanceAda = lovelace => Number(lovelace) / 1000000

  const getAddressUtxos = async address =>
    getData(`cardano/${address}/utxos`)
      .then(utxos => {
        console.debug(`There are ${utxos.length} UTxOs at address ${address}`)
        return utxos
      })

  const getScriptAddress = async script =>
    getData(`cardano/script/address?type=${script.type}&script=${script.script}`)
      .then(({ address }) => address)

  const getCardanoPKH = async address =>
    getData(`cardano/${address}/credential/payment`)
      .then(({ hash }) => hash)

  return (
    <div className="app-container">
      <Header pkh={publicKeyHash} address={walletAddress} balance={balance} />
      <div className="main-content">
        <aside className="sidebar">
          <FormSend title="Send Gift" giftAddress={scriptAddress} walletUtxos={walletUtxos} lucid={lucid} />
          <FormCollect title="Collect Gift" scriptUtxos={scriptUtxos} lucid={lucid} />
        </aside>
        <Views walletUtxos={walletUtxos} scriptUtxos={scriptUtxos} />
      </div>
    </div>
  );
}

export default App;
