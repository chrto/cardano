import './Page.css'
import React, { useEffect, useState } from 'react';
import Views from '../components/Views';
import Header from '../components/Header';
import FormSend from '../components/FormSend';
import FormCollect from '../components/FormCollect';
import getData from '../utils/getDataFromServer';
import Navbar from '../components/Navbar';
import dispatchData from '../utils/dispatchData';

const { giftScript } = require("../config.json");

function Gift({lucid, publicKeyHash, walletAddress, walletUtxos}) {
  const [scriptAddress, setScriptAddress] = useState([]);
  const [scriptUtxos, setScriptUtxos] = useState([]);

  useEffect(() => {
    Promise.all([
      getScriptAddress(giftScript)
        .then(dispatchData(setScriptAddress))
        .then(getAddressUtxos)
        .then(dispatchData(setScriptUtxos))
        .catch((err) => console.error('Fetch error:', err))
    ])
  }, []);

  const getAddressUtxos = async address =>
    getData(`cardano/${address}/utxos`)
      .then(utxos => {
        console.debug(`There are ${utxos.length} UTxOs at address ${address}`)
        return utxos
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
          <FormSend title="Send Gift" giftAddress={scriptAddress} walletUtxos={walletUtxos} lucid={lucid} />
          <FormCollect title="Collect Gift" scriptUtxos={scriptUtxos} lucid={lucid} />
        </aside>
        <Views walletUtxos={walletUtxos} scriptUtxos={scriptUtxos} />
      </div>
    </div>
  );
}

export default Gift;
