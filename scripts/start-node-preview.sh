#! /bin/bash

cardano-node run \
  --config $HOME/cardano/preview/config.json \
  --database-path $HOME/cardano/preview/db/ \
  --socket-path $HOME/cardano/preview/db/node.socket \
  --port 3001 \
  --topology $HOME/cardano/preview/topology.json \
  +RTS -N2 -I0 -A16m -qg -qb --disable-delayed-os-memory-return -RTS
