import React from 'react';
import '../App.css';

function Header({ pkh, address, balance}) {

  return (
    <div className="header">
      <div>PubKeyHash: {pkh}</div>
      <div>Address: {address}</div>
      <div>balance: {balance} Ada</div>
    </div>
  );
}

export default Header;