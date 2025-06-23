import React from 'react';
import './Header.css';

function Header({ publicKeyHash, walletAddress, walletUtxos }) {
  const getBalanceLovelace = utxos => utxos.reduce((acc, utxo) => Number(acc) + Number(utxo.assets.lovelace), 0n)
  const getBalanceAda = lovelace => Number(lovelace) / 1000000

  return (
    <div className="header">
      <div>PubKeyHash: {publicKeyHash}</div>
      <div>Address: {walletAddress}</div>
      <div>balance: {getBalanceAda(getBalanceLovelace(walletUtxos))} Ada</div>
    </div>
  );
}

export default Header;