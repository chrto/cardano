#!/bin/bash

# Default values
ADDRESS=""

# Function to display help message
function show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -a, --address <address>   Set address you want to query."
    echo "  -f, --file <path>         Set path to file with address you want to query."
    echo "  -h, --help                Show this help message and exit."
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        -a|--address)
            ADDRESS="$2"
            shift 2
            ;;
        -f|--file)
            ADDRESS="$(cat $2)"
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

if [ -z "$ADDRESS" ]; then
    >&2 echo "expected address as argument"
    show_help
    exit 1
fi
echo "address info: $ADDRESS"
cardano-cli address info --address "$ADDRESS"
