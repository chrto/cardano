// use this command to run it:  node ./nodejs_scripts/toBach32.js
const fs = require('fs');
const path = require('path');
const CSL = require('@emurgo/cardano-serialization-lib-nodejs');

// Load the .skey file (JSON with "cborHex")
const skeyPath = path.resolve('./keys/payment.skey'); // ← adjust path if needed
const skeyJson = JSON.parse(fs.readFileSync(skeyPath, 'utf8'));

// Extract raw bytes from cborHex
const cborHex = skeyJson.cborHex;
// const cborHex = "5820e0d97222b45f3f381b96c774ff42230049eaeaf81af68466d88ca8f101674285";
if (!cborHex.startsWith('5820')) {
  console.error('Unexpected CBOR format. Expected "5820" prefix.');
  process.exit(1);
}
const hex = cborHex.slice(4); // Remove "5820"
const rawBytes = Uint8Array.from(hex.match(/.{1,2}/g).map((b) => parseInt(b, 16)));

// Convert to Bech32 using cardano-serialization-lib
const privateKey = CSL.PrivateKey.from_normal_bytes(rawBytes);
const bech32 = privateKey.to_bech32();

console.log('✅ Bech32 Private Key:\n', bech32);
