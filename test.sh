#!/usr/bin/env bash

set -euo pipefail

bejnarkli=${1:-$PWD/dist-newstyle/build/*/*/bejnarkli-*/x/bejnarkli/build/bejnarkli/bejnarkli}

port=8934
outdir=blobs
password=secret
payload="Test content"
max_attempts=10
delay_between_attempts=.2

message() {
  printf '%s' "$payload" | openssl dgst -sha256 -binary -hmac "$password"
  printf '%s' "$payload"
}

send() {
  socat - "TCP4:localhost:$port"
}


tmpdir=
bejnarkli_pid=
cleanup() {
  if [[ "$bejnarkli_pid" ]];then
    kill "$bejnarkli_pid"
  fi
  if [[ "$tmpdir" && -e "$tmpdir" ]];then
    rm -rf "$tmpdir"
  fi
}
trap cleanup EXIT

tmpdir=$(mktemp -d)
cd "$tmpdir"

$bejnarkli --password "$password" &
bejnarkli_pid=$!

attempts=0
until [[ "$(message | send)" == y ]];do
  if (( attempts++ > max_attempts ));then
    exit 1
  fi
  sleep "$delay_between_attempts"
done

captured_blob_data=$(find "$outdir" -name incoming -prune -o -type f -exec cat {} +)

[[ "$captured_blob_data" == "$payload" ]]

echo "PASS"
