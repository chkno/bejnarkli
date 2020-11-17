#!/usr/bin/env bash

set -euo pipefail

bejnarkli=${1:-$PWD/dist-newstyle/build/*/*/bejnarkli-*/x/bejnarkli/build/bejnarkli/bejnarkli}

port1=8934
port2=8935
port3=8936
password=secret
payload="Test content"
max_attempts=10
delay_between_attempts=.2
tcp_timeout=1

message() {
  echo -n B
  printf '%s' "$payload" | openssl dgst -sha256 -binary -hmac "$password"
  printf '%s' "$payload"
}

send() {
  socat -t "$tcp_timeout" - "TCP4:localhost:$1"
}

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

tmpdir1=
tmpdir2=
tmpdir3=
bejnarkli_pid1=
bejnarkli_pid2=
bejnarkli_pid3=
cleanup() {
  for pid in "$bejnarkli_pid1" "$bejnarkli_pid2" "$bejnarkli_pid3";do
    if [[ "$pid" ]];then
      kill "$pid"
    fi
  done
  for tmpdir in "$tmpdir1" "$tmpdir2" "$tmpdir3";do
    if [[ "$tmpdir" && -e "$tmpdir" ]];then
      rm -rf "$tmpdir"
    fi
  done
}
trap cleanup EXIT

tmpdir1=$(mktemp -d)
tmpdir2=$(mktemp -d)
tmpdir3=$(mktemp -d)

$bejnarkli --blobdir "$tmpdir1" --password "$password" --port "$port1" &
bejnarkli_pid1=$!
$bejnarkli --blobdir "$tmpdir2" --password "$password" --port "$port2" --peer "localhost:$port1" --peer "localhost:$port3" &
bejnarkli_pid2=$!

attempts=1
until [[ "$(message | send "$port2")" == y ]];do
  if (( attempts++ >= max_attempts ));then
    echo "Couldn't send blob" >&2
    exit 1
  fi
  sleep "$delay_between_attempts"
done

# Expect blob to be present at the contacted server immediately
[[ "$(captured_blob_data "$tmpdir2")" == "$payload" ]]

# Expect blob to be present at the peer server shortly
wait_for_blob "$tmpdir1"

# Expect blob to appear at a peer server started later
$bejnarkli --blobdir "$tmpdir3" --password "$password" --port "$port3" &
bejnarkli_pid3=$!
wait_for_blob "$tmpdir3"

echo "PASS"
