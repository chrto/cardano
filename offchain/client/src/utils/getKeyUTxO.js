const getKeyUTxO = (utxo) => `${utxo.txId}#${utxo.txIndex}`

export default getKeyUTxO
