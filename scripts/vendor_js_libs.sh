set -euo pipefail

cd static/js

# npm warn deprecated axios@0.19.2: Critical security vulnerability fixed in v0.21.1. For more information, see https://github.com/axios/axios/pull/3410
npm install tendermint@5.0.2
browserify -r tendermint -s tendermint > tendermint.js

npm install @noble/curves@1.6.0
browserify -r @noble/curves/bls12-381 -s bls12_381 > bls12-381.js

npm install @chainsafe/bls-hd-key@0.3.0
browserify -r @chainsafe/bls-hd-key -s bls_hd_key > bls-hd-key.js

npm install bip39@3.1.0
browserify -r bip39 -s bip39 \
  --exclude=./wordlists/japanese.json \
  --exclude=./wordlists/spanish.json \
  --exclude=./wordlists/italian.json \
  --exclude=./wordlists/french.json \
  --exclude=./wordlists/korean.json \
  --exclude=./wordlists/czech.json \
  --exclude=./wordlists/portuguese.json \
  --exclude=./wordlists/chinese_simplified.json \
  --exclude=./wordlists/chinese_traditional.json \
  > bip39.js
