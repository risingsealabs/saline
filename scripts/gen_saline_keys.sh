#!/bin/bash
set -e
if ! command -v tendermint &> /dev/null; then
    echo "Error: tendermint is not installed."
    exit 1
fi
if ! command -v jq &> /dev/null; then
    echo "Error: jq is not installed."
    exit 1
fi
if [ -z "$1" ]; then
    echo "Usage: $0 <output_directory> [number_of_nodes]"
    exit 1
fi
OUTPUT_DIR="$1"
NUM_NODES="${2:-4}"
mkdir -p "$OUTPUT_DIR"
declare -a NODE_IDS
declare -a VALIDATOR_ADDRESSES
declare -a VALIDATOR_PUB_KEYS
for (( i=0; i<$NUM_NODES; i++ ))
do
    echo "Generating keys for node$i..."
    HOME_DIR=$(mktemp -d)
    rm -f "$HOME_DIR/config/node_key.json"
    NODE_ID=$(tendermint gen_node_key --home "$HOME_DIR" 2>&1 | tail -n1)
    NODE_KEY_JSON=$(cat "$HOME_DIR/config/node_key.json")
    NODE_IDS[$i]="$NODE_ID"
    NODE_KEY_FULL_JSON=$(jq -n \
        --arg enode "$NODE_ID" \
        --slurpfile node_key "$HOME_DIR/config/node_key.json" \
        '{enode: $enode, node_key: $node_key[0]}')
    echo "$NODE_KEY_FULL_JSON" > "$OUTPUT_DIR/node_key-$i.json"
    VALIDATOR_JSON=$(tendermint gen_validator --home "$HOME_DIR")
    VALIDATOR_KEY=$(echo "$VALIDATOR_JSON" | jq '.Key')
    LAST_SIGN_STATE=$(echo "$VALIDATOR_JSON" | jq '.LastSignState')
    VALIDATOR_FULL_JSON=$(jq -n \
        --argjson key "$VALIDATOR_KEY" \
        --argjson lastSignState "$LAST_SIGN_STATE" \
        '$key + {LastSignState: $lastSignState}')
    ADDRESS=$(echo "$VALIDATOR_KEY" | jq -r '.address')
    PUB_KEY=$(echo "$VALIDATOR_KEY" | jq '.pub_key')
    VALIDATOR_ADDRESSES[$i]="$ADDRESS"
    VALIDATOR_PUB_KEYS[$i]="$PUB_KEY"
    echo "$VALIDATOR_FULL_JSON" > "$OUTPUT_DIR/priv_validator_key-$i.json"
    rm -rf "$HOME_DIR"
done
echo "Generating genesis.json..."
GENESIS_TIME=$(date -u +"%Y-%m-%dT%H:%M:%S.%NZ")
CHAIN_ID="saline-testnet"
VALIDATORS_JSON="[]"
for (( i=0; i<$NUM_NODES; i++ ))
do
    VALIDATOR_ENTRY=$(jq -n \
        --arg address "${VALIDATOR_ADDRESSES[$i]}" \
        --argjson pub_key "${VALIDATOR_PUB_KEYS[$i]}" \
        '{
            address: $address,
            pub_key: $pub_key,
            power: "10",
            name: ""
        }')
    VALIDATORS_JSON=$(echo "$VALIDATORS_JSON" | jq --argjson val "$VALIDATOR_ENTRY" '. + [$val]')
done
jq -n \
    --arg time "$GENESIS_TIME" \
    --arg chain_id "$CHAIN_ID" \
    --argjson validators "$VALIDATORS_JSON" \
    '{
        genesis_time: $time,
        chain_id: $chain_id,
        consensus_params: {
            block: {
                max_bytes: "22020096",
                max_gas: "-1",
                time_iota_ms: "1000"
            },
            evidence: {
                max_age: "100000"
            },
            validator: {
                pub_key_types: ["ed25519"]
            }
        },
        validators: $validators,
        app_hash: ""
    }' > "$OUTPUT_DIR/genesis.json"
echo "All files have been generated in $OUTPUT_DIR"