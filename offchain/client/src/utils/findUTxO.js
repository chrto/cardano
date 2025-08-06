import getOutRefFromUTxOKey from './getOutRefFromUTxOKey';

const findUTxO = (utxos, ref) => {
  const outRef = getOutRefFromUTxOKey(ref)
  return utxos.find(utxo => utxo.txId === outRef.txHash && utxo.txIndex === outRef.outputIndex)
}

export default findUTxO
