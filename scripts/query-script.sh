#!/bin/bash

# Default values
ASSETS_DIR=
ASSETS_NAME=

# Function to display help message
function show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -d, --dir <path>          Set path to directory with script file."
    echo "  -n, --name <script_name   Set name of the script."
    echo "  -h, --help                Show this help message and exit."
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        -d|--dir)
            ASSETS_DIR="$2"
            shift 2
            ;;
        -n|--name)
            ASSETS_NAME="$2"
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

if [ -z "$ASSETS_DIR" ]; then
    >&2 echo "expected directory as argument"
    show_help
    exit 1
fi

if [ -z "$ASSETS_NAME" ]; then
    >&2 echo "expected script name as argument"
    show_help
    exit 1
fi

cardano-cli address build \
    --payment-script-file "$ASSETS_DIR/$ASSETS_NAME.plutus" \
    --testnet-magic 2 \
    --out-file "$ASSETS_DIR/$ASSETS_NAME.addr"

cardano-cli query utxo --address "$(cat $ASSETS_DIR/$ASSETS_NAME.addr)" --testnet-magic 2 $OUTPUT_FORMAT