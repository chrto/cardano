const getOutRefFromUTxOKey = (utxoKey) => {
  const chunks = utxoKey.split('#');
  const txHash = chunks[0];
  const outputIndex = parseInt(chunks[1]);
  return { txHash, outputIndex }
}

export default getOutRefFromUTxOKey
