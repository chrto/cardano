#!/bin/bash

# Default values
ASSETS_DIR=/home/devo/workspace/cardano/onchain/Vesting/assets
KEYS_PATH=/home/devo/workspace/cardano/keys
TO_ADDR=addr_test1wpu4nj5n0y4vxqjcy0msd6d396pqrkzphwt2akt6vtdk92qswzrqj
OUTPUT_NAME=deploy
VALUE=13300000
ERA=latest
NAME=""
TXIN=""
COLLATERAL=""

# Function to display help message
function show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -a, --assets <path>                 Path to the assets. Default is $ASSETS_DIR."
    echo "  -k, --keys <path>                   Path to the generated keys. Default is $KEYS_PATH."
    echo "  -o, --output <output name>          Set the name of output file name. Default is $OUTPUT_NAME"
    echo "  -e, --era <cardano era>             Set the era of cardano. Default is $ERA"
    echo "      --addr <cardano era>            Set address to which will be script deployed. Default is (burn) $TO_ADDR"
    echo "  -t, --txin <tx_i>#<tx_index>        Set the id and index of input transaction."
    echo "  -c, --collateral <tx_i>#<tx_index>  If this transaction's Plutus script fails, burn these UTXOs as collateral to pay the transaction fee."
    echo "  -n, --name <name>                   Set the name for key owner."
    echo "  -h, --help                          Show this help message and exit."
    echo ""
    echo "For more details see 'cardano-cli --help'"
    echo ""
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        -a|--assets)
            ASSETS_DIR="$2"
            shift 2
            ;;
        -k|--keys)
            KEYS_PATH="$2"
            shift 2
            ;;
        -e|--era)
            ERA="$2"
            shift 2
            ;;
        --addr)
            TO_ADDR="$2"
            shift 2
            ;;
        -t|--txin)
            TXIN="$2"
            shift 2
            ;;
        -c|--collateral)
            COLLATERAL="$2"
            shift 2
            ;;
        -n|--name)
            NAME="$2"
            shift 2
            ;;
        -h|--help)
            show_help
            exit 0
            ;;
        *)
            show_help
            exit 1
            ;;
    esac
done

if [ -z "$NAME" ]; then
    >&2 echo "expected keys owner name as argument - who will receive vest!"
    show_help
    exit 1
fi

if [ -z "$COLLATERAL" ]; then
    >&2 echo "expected collateral UTXO - which will be consumed, if validation fails!"
    show_help
    exit 1
fi

if [ -z "$TXIN" ]; then
    >&2 echo "expected input transaction id as argument - gift contract UTXO!"
    show_help
    exit 1
fi

# Build the transaction
cardano-cli $ERA transaction build \
  --testnet-magic 2 \
  --change-address "$(cat "$KEYS_PATH/$NAME.addr")" \
  --tx-in "$TXIN" \
  --tx-out "$TO_ADDR + $VALUE lovelace" \
  --tx-out-reference-script-file "$ASSETS_DIR/vesting.plutus" \
  --tx-out-inline-datum-file "$ASSETS_DIR/unit.json" \
  --out-file "$ASSETS_DIR/$OUTPUT_NAME.txbody"

# Sign the transaction
cardano-cli $ERA transaction sign \
    --tx-body-file "$ASSETS_DIR/$OUTPUT_NAME.txbody" \
    --signing-key-file "$KEYS_PATH/$NAME.skey" \
    --testnet-magic 2 \
    --out-file "$ASSETS_DIR/$OUTPUT_NAME.tx"

# Submit the transaction
cardano-cli $ERA transaction submit \
    --testnet-magic 2 \
    --tx-file "$ASSETS_DIR/$OUTPUT_NAME.tx"

tid=$(cardano-cli $ERA transaction txid --tx-file "$ASSETS_DIR/$OUTPUT_NAME.tx")
echo "transaction id: $tid"
echo "Cardanoscan: https://preview.cardanoscan.io/transaction/$tid"