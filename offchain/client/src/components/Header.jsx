import React, { useEffect, useState } from 'react';
import './Header.css';
import dispatchData from '../utils/dispatchData';

function Header({ publicKeyHash, walletAddress, walletUtxos }) {
  const [balance, setBalance] = useState(["-"]);

  useEffect(() => {
    Promise.resolve(walletUtxos)
      .then(getBalanceLovelace)
      .then(getBalanceAda)
      .then(dispatchData(setBalance))
  }, []);

  // const getBalanceLovelace = utxos => utxos.reduce((acc, utxo) => Number(acc) + Number(utxo.assets.lovelace), 0n)
  const getBalanceLovelace = utxos => {
    console.log(`wallet has ${utxos.length} utxos..`)
    return utxos.reduce((acc, utxo) => Number(acc) + Number(utxo.assets.lovelace), 0n)
  }
  const getBalanceAda = lovelace => Number(lovelace) / 1000000

  return (
    <div className="header">
      <div>PubKeyHash: {publicKeyHash}</div>
      <div>Address: {walletAddress}</div>
      <div>balance: {balance} Ada</div>
    </div>
  );
}

export default Header;