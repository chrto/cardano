import React from 'react';
import Table from './Table'
import '../App.css';
import { Data } from 'lucid-cardano'
import convertToJs from '../utils/convertToJs'

function Views({ walletUtxos, scriptUtxos }) {
  const convertUtxos = utxo => [`${utxo.txId}#${utxo.txIndex}`, utxo.assets.lovelace, Number(utxo.assets.lovelace) / 1000000]
  const convertWalletUtxos = utxo => [`${utxo.txId}#${utxo.txIndex}`, utxo.assets.lovelace, Number(utxo.assets.lovelace) / 1000000]
  const convertScriptUtxosInlineDatum = utxo => [`${utxo.txId}#${utxo.txIndex}`, utxo.datum, JSON.stringify(convertToJs(Data.from(utxo.datum)), null, 2), Number(utxo.assets.lovelace) / 1000000]
  const convertScriptUtxosHashDatum = utxo => [`${utxo.txId}#${utxo.txIndex}`, utxo.datumHash, utxo.assets.lovelace, Number(utxo.assets.lovelace) / 1000000]

  const filterInlineDatumUnit = utxo => utxo.datum === 'd87980'
  const filterInlineDatum = utxo => !!utxo.datum && !filterInlineDatumUnit(utxo)
  const filterHashDatum = utxo => !!utxo.datumHash
  const filterNoDatum = utxo => !utxo.datumHash && !utxo.datum

  return (
    <div className="tables-container">
      <Table title="Wallet Utxo's" headers={['UTXO', 'Value [Lovelace]', 'Value [Ada]']} values={walletUtxos.map(convertWalletUtxos)} />
      <Table title="Script Utxo's with Unit InLine Datum" headers={['UTXO', 'Value [Lovelace]', 'Value [Ada]']} values={scriptUtxos.filter(filterInlineDatumUnit).map(convertUtxos).sort((x, y) => y[2] - x[2])} />
      <Table title="Script Utxo's without Datum" headers={['UTXO', 'Value [Lovelace]', 'Value [Ada]']} values={scriptUtxos.filter(filterNoDatum).map(convertUtxos)} />
      <Table title="Script Utxo's with InLine Datum" headers={['UTXO', 'Datum', 'Datum Decoded', 'Value [Ada]']} values={scriptUtxos.filter(filterInlineDatum).map(convertScriptUtxosInlineDatum)} />
      <Table title="Script Utxo's with Hash Datum" headers={['UTXO', 'Datum Hash', 'Value [Lovelace]', 'Value [Ada]']} values={scriptUtxos.filter(filterHashDatum).map(convertScriptUtxosHashDatum)} />
    </div>
  );
}

export default Views;