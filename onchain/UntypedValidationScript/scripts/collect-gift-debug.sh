#!/bin/bash

# Settings
ADDRESS_OWNER=jack
SKEY_OWNER=alice
GIFT_UTXO="56a5c04bb1872632919f8f501bc9c124d04036e7fdcc6b741452ee0e0b7034c7#0"
COLLATERAL_UTXO="006e340f98777657cae0318f91b842a30723c77fb33c95b594f8eb8c99c9919d#0"

# Default values
ASSETS_DIR=/home/devo/workspace/cardano/plutus/UntypedValidationScript/assets
KEYS_PATH=/home/devo/workspace/cardano/keys
ERA=latest

# Computed values
ADDRESS=$(cat $KEYS_PATH/$ADDRESS_OWNER.addr)

# Build the transaction
cardano-cli $ERA transaction build \
    --testnet-magic 2 \
    --tx-in "$GIFT_UTXO" \
    --tx-in-script-file "$ASSETS_DIR/gift.plutus" \
    --tx-in-inline-datum-present \
    --tx-in-redeemer-file "$ASSETS_DIR/unit.json" \
    --tx-in-collateral "$COLLATERAL_UTXO" \
    --change-address $ADDRESS \
    --out-file "$ASSETS_DIR/collect-gift.txbody"

# # Sign the transaction
cardano-cli $ERA transaction sign \
    --tx-body-file "$ASSETS_DIR/collect-gift.txbody" \
    --signing-key-file "$KEYS_PATH/$SKEY_OWNER.skey" \
    --testnet-magic 2 \
    --out-file "$ASSETS_DIR/collect-gift.tx"

echo ""
# Submit the transaction
cardano-cli $ERA transaction submit \
    --testnet-magic 2 \
    --tx-file "$ASSETS_DIR/collect-gift.tx"

echo ""

tid=$(cardano-cli $ERA transaction txid --tx-file "$ASSETS_DIR/collect-gift.tx")
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"