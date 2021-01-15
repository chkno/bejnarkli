#!/usr/bin/env bash

set -euo pipefail

bejnarkli=${1:-$PWD/dist-newstyle/build/*/*/bejnarkli-*/x/bejnarkli/build/bejnarkli/bejnarkli}
bejnarkli_send=${2:-$PWD/dist-newstyle/build/*/*/bejnarkli-*/x/bejnarkli-send/build/bejnarkli-send/bejnarkli-send}

port1=8934
port2=8935
port3=8936
port4=8937
payload="Test content"
max_attempts=10
delay_between_attempts=.2
tcp_timeout=1

captured_blob_data() {
  find "$1" -name '.*' -prune -o -type f -exec cat {} +
}

wait_for_blob() {
  attempts=1
  until [[ "$(captured_blob_data "$1")" == "$payload" ]];do
    if (( attempts++ >= max_attempts ));then
      echo "Blob didn't replicate to peer" >&2
      exit 1
    fi
    sleep "$delay_between_attempts"
  done
}

passwordfile=
blob=
tmpdir1=
tmpdir2=
tmpdir3=
tmpdir4=
bejnarkli_pid1=
bejnarkli_pid2=
bejnarkli_pid3=
bejnarkli_pid4=
cleanup() {
  for pid in "$bejnarkli_pid1" "$bejnarkli_pid2" "$bejnarkli_pid3" "$bejnarkli_pid4";do
    if [[ "$pid" ]];then
      kill "$pid"
    fi
  done
  for tmp in "$passwordfile" "$blob" "$tmpdir1" "$tmpdir2" "$tmpdir3" "$tmpdir4";do
    if [[ "$tmp" && -e "$tmp" ]];then
      rm -rf "$tmp"
    fi
  done
}
trap cleanup EXIT

passwordfile=$(mktemp)
blob=$(mktemp)
tmpdir1=$(mktemp -d)
tmpdir2=$(mktemp -d)
tmpdir3=$(mktemp -d)
tmpdir4=$(mktemp -d)

echo secret > "$passwordfile"


$bejnarkli --blobdir "$tmpdir1" --passwordfile "$passwordfile" --port "$port1" --listenaddress localhost &
bejnarkli_pid1=$!
$bejnarkli --blobdir "$tmpdir2" --passwordfile "$passwordfile" --port "$port2" --peer "localhost:$port1" --peer "localhost:$port3" &
bejnarkli_pid2=$!

echo "$payload" > "$blob"
$bejnarkli_send --passwordfile "$passwordfile" "$blob" "localhost:$port2"

# Expect blob to be present at the contacted server immediately
[[ "$(captured_blob_data "$tmpdir2")" == "$payload" ]]

# Expect blob to be present at the peer server shortly
wait_for_blob "$tmpdir1"

# Expect blob to appear at a peer server started later
$bejnarkli --blobdir "$tmpdir3" --passwordfile "$passwordfile" --port "$port3" &
bejnarkli_pid3=$!
wait_for_blob "$tmpdir3"

# Restart a server with a new peer & expect the blob to be forwarded
kill "$bejnarkli_pid3"
wait "$bejnarkli_pid3" || true
$bejnarkli --blobdir "$tmpdir3" --passwordfile "$passwordfile" --port "$port3" --peer "localhost:$port4" &
bejnarkli_pid3=$!
$bejnarkli --blobdir "$tmpdir4" --passwordfile "$passwordfile" --port "$port4" &
bejnarkli_pid4=$!
wait_for_blob "$tmpdir4"


echo "PASS"
