import './Page.css'
import React, { useEffect, useState } from 'react';
import ViewsGift from '../components/ViewsGift';
import Header from '../components/Header';
import FormGiftSend from '../components/FormGiftSend';
import FormGiftCollect from '../components/FormGiftCollect';
import getData from '../utils/getDataFromServer';
import Navbar from '../components/Navbar';
import dispatchData from '../utils/dispatchData';
import useSafeInterval from '../utils/useSafeInterval';

const { giftScript, apiRefreshDelay } = require("../config.json");

function Gift({publicKeyHash, walletAddress, walletUtxos}) {
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
      .catch((err) => console.error('Fetch error:', err))
  const getScritptByName = (name) => ({
    name, script: giftScript
  })

  const getAddressUtxos = async address =>
    getData(`cardano/${address}/utxos`)
      .then(utxos => {
        console.debug(`There are ${utxos.length} UTxOs at address ${address}`)
        return utxos
      })
      .catch(e => {
        console.error(`Can not fetch utxos for address ${address} from server!\n origin: ${e.message}`)
        return scriptUtxos;
      })

  const getScriptAddress = async script =>
    getData(`cardano/script/address?type=${script.type}&script=${script.script}`)
      .then(({ address }) => address)

  return (
    <div className="app-container">
      <Navbar />
      <Header publicKeyHash={publicKeyHash} walletAddress={walletAddress} walletUtxos={walletUtxos} />
      <div className="main-content">
        <aside className="sidebar">
          <FormGiftSend title="Send Gift" scriptAddress={scriptAddress} walletUtxos={walletUtxos} />
          <FormGiftCollect title="Collect Gift" scriptUtxos={scriptUtxos} validatorScript={ giftScript } />
        </aside>
        <ViewsGift walletUtxos={walletUtxos} scriptUtxos={scriptUtxos} scriptAddress={scriptAddress} selectedScript={selectedScript} />
      </div>
    </div>
  );
}

export default Gift;
