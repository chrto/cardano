import './Page.css'
import React, { useEffect, useState } from 'react';
import ViewsFortyTwo from '../components/ViewsFortyTwo';
import Header from '../components/Header';
import ChoiceFortyTwo from '../components/ChoiceFortyTwo';
import FormFortyTwoSend from '../components/FormFortyTwoSend';
import FormFortyTwoCollect from '../components/FormFortyTwoCollect';
import getData from '../utils/getDataFromServer';
import Navbar from '../components/Navbar';
import dispatchData from '../utils/dispatchData';
import useSafeInterval from '../utils/useSafeInterval';

const {fortyTwoScript, fortyTwoTypedScript, fortyTwoTypedScriptP, apiRefreshDelay} = require("../config.json");

function FortyTwo({publicKeyHash, walletAddress, walletUtxos}) {
  const [scriptAddress, setScriptAddress] = useState("...");
  const [scriptUtxos, setScriptUtxos] = useState([]);
  const [selectedScript, setSelectedScript] = useState({ name: "fortyTwoScript", script: fortyTwoScript });
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
      case 'fortyTwoScript': return {name: "fortyTwoScript", script: fortyTwoScript};
      case 'fortyTwoTypedScript': return {name: "fortyTwoTypedScript", script: fortyTwoTypedScript};
      case 'fortyTwoTypedScriptP': return {name: "fortyTwoTypedScriptP", script: fortyTwoTypedScriptP};
      default: return {name: "fortyTwoScript", script: fortyTwoScript};
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
          <ChoiceFortyTwo handleChoice={handleChoice} selected={selectedScript.name}/>
          <FormFortyTwoSend title="Send FortyTwo" scriptAddress={scriptAddress} walletUtxos={walletUtxos} />
          <FormFortyTwoCollect title="Collect FortyTwo" scriptUtxos={scriptUtxos} validatorScript={selectedScript.script} />
        </aside>
        <ViewsFortyTwo walletUtxos={walletUtxos} scriptUtxos={scriptUtxos} scriptAddress={scriptAddress} selectedScript={selectedScript} />
      </div>
    </div>
  );
}

export default FortyTwo;
