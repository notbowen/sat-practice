#!/bin/sh
set -eu

curl --fail --silent --show-error --max-time 4 \
  http://127.0.0.1:"${SAT_PORT:-8080}"/healthz >/dev/null

if [ "${SAT_TUNNEL_ENABLED:-true}" = "true" ]; then
  curl --fail --silent --show-error --max-time 4 \
    http://127.0.0.1:2000/ready >/dev/null
fi
