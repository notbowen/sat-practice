# syntax=docker/dockerfile:1

FROM cloudflare/cloudflared:2026.7.3 AS cloudflared

FROM ocaml/opam:debian-12-ocaml-5.4 AS build

ARG DEBIAN_FRONTEND=noninteractive
USER 0
# hadolint ignore=DL3008
RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
      libargon2-dev \
      libev-dev \
      libffi-dev \
      libgmp-dev \
      libssl-dev \
      libsqlite3-dev \
      pkg-config \
    && rm -rf /var/lib/apt/lists/*

USER 1000:1000
WORKDIR /home/opam/app

# Keep dependency installation cached until the opam manifest changes.
COPY --chown=1000:1000 sat.opam dune-project ./
RUN opam install . --deps-only --yes

COPY --chown=1000:1000 bin ./bin
COPY --chown=1000:1000 lib ./lib
COPY --chown=1000:1000 static ./static
RUN opam exec -- dune build --profile=release bin/main.exe


FROM debian:12-slim AS runtime

ARG DEBIAN_FRONTEND=noninteractive
# hadolint ignore=DL3008
RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
      ca-certificates \
      curl \
      libargon2-1 \
      libev4 \
      libffi8 \
      libgmp10 \
      libssl3 \
      libsqlite3-0 \
      netbase \
    && rm -rf /var/lib/apt/lists/* \
    && groupadd --gid 10001 sat \
    && useradd --uid 10001 --gid 10001 --no-create-home --home-dir /nonexistent --shell /usr/sbin/nologin sat \
    && mkdir -p /opt/sat/static /data \
    && chown sat:sat /data

COPY --from=build /home/opam/app/_build/default/bin/main.exe /opt/sat/sat
COPY --from=build /home/opam/app/static /opt/sat/static
COPY --from=cloudflared /usr/local/bin/cloudflared /usr/local/bin/cloudflared
COPY --chmod=0755 docker/entrypoint.sh /usr/local/bin/sat-entrypoint
COPY --chmod=0755 docker/healthcheck.sh /usr/local/bin/sat-healthcheck

WORKDIR /opt/sat
USER 10001:10001

ENV SAT_HOST=127.0.0.1 \
    SAT_PORT=8080 \
    SAT_DB_PATH=/data/sat.db \
    SAT_COOKIE_SECURE=true \
    SAT_TUNNEL_ENABLED=true \
    TUNNEL_PROTOCOL=auto \
    CLOUDFLARED_METRICS=127.0.0.1:2000 \
    LD_PRELOAD=libargon2.so.1

VOLUME ["/data"]

HEALTHCHECK --interval=30s --timeout=5s --start-period=60s --retries=3 \
  CMD ["/usr/local/bin/sat-healthcheck"]

ENTRYPOINT ["/usr/local/bin/sat-entrypoint"]
