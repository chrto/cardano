#!/bin/bash

# Default values
KEYS_PATH=/home/devo/workspace/cardano/keys
NAME=""

# Function to display help message
function show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -d, --dir <path>          Path to the generated keys. Default is $KEYS_PATH."
    echo "  -n, --name <name>         Set the name for key owner."
    echo "  -h, --help                Show this help message and exit."
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        -d|--dir)
            KEYS_PATH="$2"
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
    >&2 echo "expected name as argument"
    show_help
    exit 1
fi

mkdir -p "$KEYS_PATH"

VKEY="$KEYS_PATH/$NAME.vkey"
SKEY="$KEYS_PATH/$NAME.skey"
ADDR="$KEYS_PATH/$NAME.addr"

if [ -f "$VKEY" ]; then
    >&2 echo "verification key file $VKEY already exists"
    exit 1
fi

if [ -f "$SKEY" ]; then
    >&2 echo "signing key file $SKEY already exists"
    exit 1
fi

if [ -f "$ADDR" ]; then
    >&2 echo "address file $ADDR already exists"
    exit 1
fi

cardano-cli address key-gen --verification-key-file "$VKEY" --signing-key-file "$SKEY" &&
cardano-cli address build --payment-verification-key-file "$VKEY" --testnet-magic 2 --out-file "$ADDR"

echo "wrote verification key to: $VKEY"
echo "wrote signing key to: $SKEY"
echo "wrote address to: $ADDR"