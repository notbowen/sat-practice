#!/bin/sh
set -eu

app_pid=""
tunnel_pid=""

stop_children() {
  trap - TERM INT HUP
  if [ -n "$tunnel_pid" ]; then
    kill "$tunnel_pid" 2>/dev/null || true
  fi
  if [ -n "$app_pid" ]; then
    kill "$app_pid" 2>/dev/null || true
  fi
  if [ -n "$tunnel_pid" ]; then
    wait "$tunnel_pid" 2>/dev/null || true
  fi
  if [ -n "$app_pid" ]; then
    wait "$app_pid" 2>/dev/null || true
  fi
}

# Invoked indirectly by the signal traps below.
# shellcheck disable=SC2329
handle_signal() {
  stop_children
  exit 143
}

trap handle_signal TERM INT HUP

if [ "${SAT_TUNNEL_ENABLED:-true}" != "true" ]; then
  exec env -u TUNNEL_TOKEN /opt/sat/sat
fi

if [ -z "${TUNNEL_TOKEN:-}" ]; then
  echo "TUNNEL_TOKEN is required when SAT_TUNNEL_ENABLED=true." >&2
  exit 64
fi

# The application does not need access to the tunnel credential.
env -u TUNNEL_TOKEN /opt/sat/sat &
app_pid=$!

app_ready=false
for _attempt in $(seq 1 60); do
  if curl --fail --silent http://127.0.0.1:"${SAT_PORT:-8080}"/healthz >/dev/null; then
    app_ready=true
    break
  fi
  if ! kill -0 "$app_pid" 2>/dev/null; then
    set +e
    wait "$app_pid"
    status=$?
    set -e
    echo "SAT application exited before becoming ready." >&2
    exit "$status"
  fi
  sleep 1
done

if [ "$app_ready" != "true" ]; then
  echo "SAT application did not become ready within 60 seconds." >&2
  stop_children
  exit 1
fi

cloudflared tunnel \
  --no-autoupdate \
  --loglevel "${TUNNEL_LOGLEVEL:-info}" \
  --metrics "${CLOUDFLARED_METRICS:-127.0.0.1:2000}" \
  --protocol "${TUNNEL_PROTOCOL:-auto}" \
  run &
tunnel_pid=$!

while kill -0 "$app_pid" 2>/dev/null && kill -0 "$tunnel_pid" 2>/dev/null; do
  sleep 2
done

set +e
if ! kill -0 "$app_pid" 2>/dev/null; then
  wait "$app_pid"
  status=$?
  echo "SAT application exited; stopping tunnel." >&2
else
  wait "$tunnel_pid"
  status=$?
  echo "cloudflared exited; stopping application." >&2
fi
set -e

stop_children
exit "$status"
