const utxoToLucid = (utxo) => ({
  ...utxo,
  outputIndex: utxo.txIndex,
  txHash: utxo.txId
})

export default utxoToLucid
