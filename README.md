# Moongle

Moongle indexes Moonbit packages and provides fast, structure-aware search by name or type. It exposes a CLI and an HTTP API backed by PostgreSQL full‑text search.

- Name search: by symbol or fully qualified path
- Type search: structural matching on parameter and return types, with effects (async/raise)
- Data source: Mooncakes registry + Moonbit core library
- Interfaces: CLI and Servant HTTP server

## Contents
- Quick start
- Configuration
- Indexing data
- Running the server
- CLI usage
- HTTP API
- Query syntax
- Notes & limitations
- Development
- License

## Quick start

Prerequisites:
- PostgreSQL 13+ running locally (defaults: host=localhost port=5432 db=postgres user=postgres password="")
- GHC 9.12.x and cabal-install, or Nix with flakes
- Moonbit toolchain installed (to provide core library at `~/.moon/lib/core`)

Build:
- Cabal: `cabal build moongle:exe:moongle`
- Nix: `nix build .#moongle:exe:moongle`

Run:
- Cabal: `cabal run moongle:exe:moongle`
- Nix: `nix run .#moongle:exe:moongle`

## Configuration

At runtime Moongle composes configuration from defaults, `~/.config/moongle/config.yaml` and CLI flags.

Supported keys:
- registryUrl: string (default: https://mooncakes.io/assets/modules.json)
- mooncakesBaseUrl: string (default: https://moonbitlang-mooncakes.s3.us-west-2.amazonaws.com/user)
- moongleStoragePath: string (by default `~/.moongle` is used)
- parallel: number (default: 32)
- dbHost, dbPort, dbName, dbUser, dbPassword

Example `~/.config/moongle/config.yaml` (JSON):
```yaml
dbHost: localhost
dbName: postgres
dbPassword: "0528"
dbUser: hank
parallel: 32
```

CLI flags override config: `--db_host`, `--db_port`, `--db_name`, `--db_user`, `--db_password`, `--config`.

## Indexing data

1) Initialize/refresh the index (downloads modules and core, parses .mbti, writes to Postgres):
- `moongle update [-u <registry-url>] [--jobs <N>]`

Notes:
- Migrations currently drop and recreate the `defs` table.
- Indexed entities: top-level functions and trait methods from parsed `.mbti` files.

## Running the server

- Start HTTP API: `moongle serve -h 0.0.0.0 -p 8080`
- Default base path: `/api`

## CLI usage

- Search by type: `moongle search -q "(Int) -> String" -l 10`
- Search by type with generics and effects: `moongle search -q "async [K: Compare, V](K, V) -> Int raise E"`
- Search by name (any module): `moongle search -q "insert"`
- Search by qualified name: `moongle search -q "@btree/btree.insert"`

## HTTP API

Base path: `/api`

- POST `/api/search`
  - Request body:
    ```json
    { "dat": { "q": "(Int) -> String", "limit": 20 } }
    ```
  - Response body (on success):
    ```json
    {
      "status": "ok",
      "dat": {
        "hitsTotal": 2,
        "items": [
          { "user": "...", "mod": "...", "package": ["..."], "decl": "fn sig", "score": 0 }
        ]
      },
      "err": null
    }
    ```

- GET `/api/stats`
  - Returns counts of indexed declarations and packages, plus `lastIndexed` timestamp.

## Query syntax

Moongle supports two forms: name queries and type queries.

- Name query
  - `name` — match by symbol name in any module (exact symbol match)
  - `@pkgPath/module.name` — restrict to a module path. `pkgPath` is slash‑separated; then a dot before `name`.
  - Examples: `insert`, `@btree/btree.insert`

- Type query
  - Form: `[TypeParams] (ParamTypes) -> ReturnType [effects]`
  - Optional pieces:
    - `async` — match async functions
    - type parameters: `[K, V]` or with constraints `[K: Compare, V]`
    - exception effect: `raise E` for a concrete exception type, or `raise ?` for polymorphic
  - Examples:
    - `(Int) -> String`
    - `[K, V](K, V) -> Int`
    - `async [K: Compare, V](K, V) -> Int raise E`

Matching semantics
- Structural on types (generic type variables are positionally unified)
- Name and type components use prefix match on module paths (e.g., `pkg.module.Type`)
- Indexed effects: `async` and `raise` only

## Notes & limitations
- Only function declarations and trait methods are currently indexed.
- Dynamic trait types are not yet encoded in the index.
- Running `update` recreates the `defs` table (drops existing data).

## Development

- Build: `cabal build all`
- Run tests: add tests under `test/` (none yet)
- Architecture: see `docs/Architecture.md`
- Useful modules:
  - Query AST and parser: `src/Moongle/Query/Syntax.hs`, `src/Moongle/Query/Parser.hs`
  - Tokenization/search: `src/Moongle/TypeSearch.hs`
  - Database schema and queries: `src/Moongle/DB.hs`
  - CLI/server entrypoints: `src/Moongle/CLI.hs`, `src/Moongle/App.hs`, `src/Moongle/Server.hs`

Experimental UIs
- Web (Vue): `web/`
- Elm prototype: `elm/`

## License
MIT. See `LICENSE`.

![Alt](https://repobeats.axiom.co/api/embed/1b89702cbea6a0cfd0b04409ed3abee5e8d0d2ec.svg "Repobeats analytics image")
