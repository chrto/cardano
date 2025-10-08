#!/bin/bash

# Default values
ASSETS_DIR=/home/devo/workspace/cardano/onchain/UntypedValidationScript/assets
KEYS_PATH=/home/devo/workspace/cardano/keys
BURN_VALUE=1000000
OUTPUT_NAME=burn
ERA=latest
NAME=""
TXIN=""

# Function to display help message
function show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -a, --assets <path>                 Path to the assets. Default is $ASSETS_DIR."
    echo "  -k, --keys <path>                   Path to the generated keys. Default is $KEYS_PATH."
    echo "  -v, --value <amount of lovelace>    Set the value in lovelace, to send to gift contract. Defoult is $BURN_VALUE"
    echo "  -o, --output <output name>          Set the name of output file name. Default is $OUTPUT_NAME"
    echo "  -e, --era <cardano era>             Set the era of cardano. Default is $ERA"
    echo "  -n, --name <name>                   Set the name for key owner."
    echo "  -t, --txin <tx_id tx_index>         Set the id and index of input transaction."
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
        -v|--value)
            BURN_VALUE="$2"
            shift 2
            ;;
        -o|--output)
            OUTPUT_NAME="$2"
            shift 2
            ;;
        -e|--era)
            ERA="$2"
            shift 2
            ;;
        -n|--name)
            NAME="$2"
            shift 2
            ;;
        -t|--txin)
            TXIN="$2"
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
    >&2 echo "expected keys owner name as argument"
    show_help
    exit 1
fi

if [ -z "$TXIN" ]; then
    >&2 echo "expected input transaction id as argument"
    show_help
    exit 1
fi

# Build burn address
cardano-cli address build \
    --payment-script-file "$ASSETS_DIR/$OUTPUT_NAME.plutus" \
    --testnet-magic 2 \
    --out-file "$ASSETS_DIR/$OUTPUT_NAME.addr"

# Build the transaction
cardano-cli $ERA transaction build \
    --testnet-magic 2 \
    --tx-in "$TXIN" \
    --tx-out "$(cat "$ASSETS_DIR/$OUTPUT_NAME.addr") + $BURN_VALUE lovelace" \
    --tx-out-inline-datum-file "$ASSETS_DIR/unit.json" \
    --change-address "$(cat "$KEYS_PATH/$NAME.addr")" \
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