# Local SAT Revision Tool

A private SAT practice app written in OCaml. It pulls live question content from College Board's educator question bank, stores only metadata and learner progress in SQLite, and permanently retires questions once the learner answers them correctly. Native development stays localhost-only; the production container supports private Cloudflare Tunnel ingress.

The generator mirrors the real digital SAT's structure, timing, and higher-difficulty second-module route:

- Reading & Writing: 27 questions per module in 32 minutes — Module 1 has 9 easy, 9 medium, and 9 hard; Module 2 has 4 easy, 10 medium, and 13 hard
- Math: 22 questions per module in 35 minutes — Module 1 has 7 easy, 8 medium, and 7 hard; Module 2 has 3 easy, 8 medium, and 11 hard
- A 10-minute break separates the Reading & Writing and Math sections

Module 1 retains a broad routing mix, while Module 2 deliberately practices the higher route associated with access to the full 800-point section scale. College Board does not publish exact difficulty counts for that route, so these Module 2 ratios are an explicit practice approximation. The original SAT domain proportions are retained. When cached metadata identifies enough Math student-produced-response questions, the generator prefers six of them without changing a domain/difficulty quota.

## Run

Install the opam dependencies in the project switch, then:

```sh
opam install . --deps-only --with-test
opam exec -- dune build
opam exec -- dune exec sat
```

Open [http://127.0.0.1:8080](http://127.0.0.1:8080). On first startup, the metadata refresh can take a few seconds.

Configuration:

| Variable | Default | Purpose |
|---|---|---|
| `SAT_HOST` | `127.0.0.1` | Listening interface |
| `SAT_PORT` | `8080` | Listening port |
| `SAT_DB_PATH` | `./var/sat.db` | SQLite database path |
| `SAT_COOKIE_SECURE` | `false` | Add `Secure` to cookies when served through HTTPS |

Run the test suite with:

```sh
opam exec -- dune runtest
```

## Docker

Build and run the production container locally:

```sh
docker build -t sat-revision .
docker run --rm -p 8080:8080 \
  -e SAT_TUNNEL_ENABLED=false \
  -e SAT_HOST=0.0.0.0 \
  -e SAT_COOKIE_SECURE=false \
  -v sat-data:/data \
  sat-revision
```

The production default is tunnel-only: the application binds to
`127.0.0.1:8080`, `cloudflared` is supervised in the same container, and the
container exits if either process dies. Direct local access therefore requires
the three overrides above. The container runs as an unprivileged user, exposes
`GET /healthz` internally, and stores SQLite state at `/data/sat.db`. The build
context explicitly excludes the local `var/` directory, so local accounts and
progress are never copied into the image.

Container-only configuration:

| Variable | Default | Purpose |
|---|---|---|
| `SAT_TUNNEL_ENABLED` | `true` | Run the app behind the supervised tunnel |
| `TUNNEL_TOKEN` | none | Remotely managed Cloudflare Tunnel token; required in tunnel mode |
| `TUNNEL_PROTOCOL` | `auto` | Cloudflare Tunnel transport selection |
| `TUNNEL_LOGLEVEL` | `info` | `cloudflared` log verbosity |
| `CLOUDFLARED_METRICS` | `127.0.0.1:2000` | Metrics and readiness listener |

For Fly.io, mount a persistent volume at `/data`. SQLite should be run on a
single application machine unless you introduce database-level replication;
Fly volumes are machine-local and are not shared between replicas.

### Cloudflare Tunnel and Fly.io

The included `fly.toml` intentionally defines no Fly `http_service` or
`services`. As a result, Fly Proxy has no route to the application; the
outbound Cloudflare Tunnel is the only public ingress path.

1. In Cloudflare, create a remotely managed tunnel and add a public hostname.
   Set its service URL to `http://localhost:8080`.
2. Enable the desired Cloudflare WAF managed rules, custom rules, and rate
   limits for that hostname. `cloudflared` supplies the private origin path;
   WAF enforcement occurs at Cloudflare's edge.
3. Prepare the Fly configuration and persistent volume:

   ```sh
   # Edit app and primary_region in fly.toml before continuing.
   fly volumes create sat_data --region sin
   fly secrets set TUNNEL_TOKEN='<token copied from the Cloudflare tunnel>'
   fly deploy
   fly scale count 1
   ```

4. Do not add `[http_service]` or `[[services]]` to `fly.toml`. If Fly Launch
   allocated public IPs during earlier setup, they are unnecessary for this
   tunnel-only configuration.

The tunnel token is read from `TUNNEL_TOKEN` and is never placed in the image or
the `cloudflared` process arguments. Fly's top-level health check uses
cloudflared's `/ready` endpoint on port `2000` and only passes once the tunnel
has an active Cloudflare connection.

## Behavior

- Select any combination of Reading & Writing 1/2 and Math 1/2.
- Timers are enforced by server-side deadlines. Closing the browser does not pause a module, and an automatic worker locks expired modules.
- Once a Reading & Writing module is submitted, Math modules unlock only after a 10-minute break, like the real test.
- Question MathML is typeset by a vendored MathJax build (`static/mathjax/`), so the strict content-security-policy stays `script-src 'self'`.
- Practice sets can be deleted from the dashboard or the set page; deletion cascades to their modules, answers, and scores.
- Answers and review flags autosave. Correctness and rationales remain hidden until submission.
- Correct questions become `done` and never return for that user. Wrong and skipped questions remain eligible alongside unseen questions.
- Section estimates appear only after both modules in that section are completed. A total appears only after both complete sections. Estimates use the published SAT Practice Test 8 fixed-form score ranges and are explicitly unofficial.
- If live question content is unavailable during grading, responses remain locked in `grading_pending` and are retried safely.

## Storage and security

SQLite uses foreign keys, WAL mode, a busy timeout, ownership indexes, and transactions for grading/progress updates. Passwords use Argon2id. Sessions are opaque, database-backed, seven-day cookies with `HttpOnly` and `SameSite=Strict`; state-changing routes require CSRF tokens. Login attempts are throttled in memory.

Question prompts, choices, keys, and rationales are sanitized and held only in a bounded in-memory cache. SQLite retains question identifiers and metadata, not the full question content.

## Content notice

This tool is intended for private, local, noncommercial study. It is not affiliated with or endorsed by College Board. College Board content remains subject to its terms; do not publish, redistribute, or use this app to bulk-export question content.
