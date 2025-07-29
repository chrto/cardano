const findUTxO = (utxos, ref) => {
  const chunks = ref.split('#');
  const txId = chunks[0];
  const txIndex = parseInt(chunks[1]);
  return utxos.find(utxo => utxo.txId === txId && utxo.txIndex === txIndex)
}

export default findUTxO
