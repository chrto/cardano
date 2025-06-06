#!/bin/bash

# Default values
VKEY=""

# Function to display help message
function show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -f, --file <path>         Set path to file with verification key. It is your .vkey file."
    echo "  -h, --help                Show this help message and exit."
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        -f|--file)
            VKEY=$2
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

if [ -z "$VKEY" ]; then
    >&2 echo "expected verification key file as argument"
    show_help
    exit 1
fi
echo "PubKey Hash:"
cardano-cli address key-hash  --payment-verification-key-file $VKEY
