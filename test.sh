#!/usr/bin/env bash

set -euo pipefail

bejnarkli=${1:-$PWD/dist-newstyle/build/*/*/bejnarkli-*/x/bejnarkli/build/bejnarkli/bejnarkli}

port1=8934
port2=8935
password=secret
payload="Test content"
max_attempts=10
delay_between_attempts=.2

message() {
  echo -n B
  printf '%s' "$payload" | openssl dgst -sha256 -binary -hmac "$password"
  printf '%s' "$payload"
}

send() {
  socat - "TCP4:localhost:$1"
}

captured_blob_data() {
  find "$1" -name incoming -prune -o -type f -exec cat {} +
}


tmpdir1=
tmpdir2=
bejnarkli_pid1=
bejnarkli_pid2=
cleanup() {
  for pid in "$bejnarkli_pid1" "$bejnarkli_pid2";do
    if [[ "$pid" ]];then
      kill "$pid"
    fi
  done
  for tmpdir in "$tmpdir1" "$tmpdir2";do
    if [[ "$tmpdir" && -e "$tmpdir" ]];then
      rm -rf "$tmpdir"
    fi
  done
}
trap cleanup EXIT

tmpdir1=$(mktemp -d)
tmpdir2=$(mktemp -d)

$bejnarkli --blobdir "$tmpdir1" --password "$password" --port "$port1" &
bejnarkli_pid1=$!
$bejnarkli --blobdir "$tmpdir2" --password "$password" --port "$port2" --peer "localhost:$port1" &
bejnarkli_pid2=$!

attempts=0
until [[ "$(message | send "$port2")" == y ]];do
  if (( attempts++ > max_attempts ));then
    echo "Couldn't send blob" >&2
    exit 1
  fi
  sleep "$delay_between_attempts"
done

[[ "$(captured_blob_data "$tmpdir2")" == "$payload" ]]

attempts=0
until [[ "$(captured_blob_data "$tmpdir1")" == "$payload" ]];do
  if (( attempts++ > max_attempts ));then
    echo "Blob didn't replicate" >&2
    exit 1
  fi
  sleep "$delay_between_attempts"
done

echo "PASS"
