# Future Refactor

> Goal: Model Moongle’s **search and indexing** logic as a high‑level **domain DSL** and interpret it into a small set of **capability DSLs** (effects). This yields replaceable backends, strong testability, and clear observability while keeping business logic pure and easy to reason about.

---

## 1. Core Idea (Why)

**High‑level language → set of lower‑level languages.** Example from the slides:

```
DeactivateAccount = DropAllRights >> AuthSuspend >> DeleteCrmUser
```

In Moongle we do:

```
Resolve q = PlanQuery q >> Lookup >> Rank >> (PutCache)
```

**Benefits**

* **Capability boundaries** are explicit in types (e.g., `Parser :> es, IndexStore :> es`).
* **Replaceable runtimes** (Postgres/Redis/Memory/SQLite/DuckDB) with zero changes to business logic.
* **Testability** via pure interpreters and in‑memory backends.
* **Observability** via a Telemetry effect; **Explain** comes for free: provide an interpreter that builds a plan without executing.

---

## 2. Terms & Conventions (What)

* **Effect**: composable capability interface, e.g., `Parser`, `IndexStore`, `Cache`, `Ranker`, `Telemetry`.
* **Interpreter**: maps an effect to a concrete implementation (PG/Redis/pure/log-only).
* **High‑level DSL**: business use cases, e.g., `SearchApp`, `IndexApp`.
* **Assembly**: runtime wiring order of interpreters.

We recommend the Haskell library **`effectful`** (others are fine).

---

## 3. High‑level DSLs (Business Use Cases)

### 3.1 `SearchApp`

Domain types (illustrative):

* `newtype RawQuery = RawQuery Text`
* `newtype Plan = Plan Text` (can be a structured AST later)
* `data Hit = Hit { defId :: Int, score0 :: Double }`
* `data Result = Result { defId :: Int, score :: Double }`

Actions:

* `Resolve :: RawQuery -> [Result]` — execute a search
* `Explain :: RawQuery -> Plan` — produce the plan only

### 3.2 `IndexApp`

Actions (examples):

* `ReindexPackage :: Pkg -> ()`
* `BulkReindex :: [Pkg] -> ()`
* `VacuumIndex :: () -> ()`

---

## 4. Capability DSLs (Lower‑level Interfaces)

* **Parser**: `PlanQuery :: RawQuery -> Plan` (Moonbit parsing/rewriting; pure when possible)
* **IndexStore**: `Lookup :: Plan -> [Hit]`, `UpsertDefs :: [Def] -> ()` (logical interface; keep SQL out of the type)
* **Ranker**: `Rank :: RawQuery -> [Hit] -> [Result]`
* **Cache**: `GetCached/PutCached`
* **Telemetry**: `Count/Observe/Timing` (Prometheus/StatsD/OTel)
* (Index pipeline) **Fetcher/Extractor/Tokenizer**: fetch code, extract declarations, tokenize
* (Cross-cutting) **Clock/RateLimit/Auth/MetricsContext**

> Guideline: capability interfaces describe *what*; backends encode *how*.

---

## 5. Interpreters: High‑level → Capabilities (How)

### 5.1 `SearchApp` → `[Parser, IndexStore, Ranker, Cache, Telemetry]`

Skeleton:

```haskell
runSearchApp
  :: ( Parser :> es
     , IndexStore :> es
     , Ranker :> es
     , Cache :> es
     , Telemetry :> es
     )
  => Eff (SearchApp : es) a -> Eff es a
runSearchApp = interpret $ \_ -> \case
  Resolve q -> do
    Telemetry.count "search.request" 1
    mc <- Cache.get (key q)
    case mc of
      Just rs -> pure rs
      Nothing -> do
        plan <- Parser.plan q
        hits <- IndexStore.lookup plan
        rs   <- Ranker.rank q hits
        Cache.put (key q) rs
        pure rs
  Explain q -> Parser.plan q
```

### 5.2 `IndexApp` → `[Fetcher, Extractor, Tokenizer, IndexStore, Telemetry]`

```
ReindexPackage pkg =
  FetchSource pkg
  >> ExtractDefs
  >> Tokenize
  >> UpsertDefs
  >> EmitMetrics
```

---

## 6. Runtimes (Assemblies)

* **Server (HTTP)**

  * `Parser -> Moonbit` (pure)
  * `IndexStore -> Postgres`
  * `Cache -> Redis`
  * `Ranker -> Default/ML`
  * `Telemetry -> Prom/OTel`

* **CLI (offline/local)**

  * `IndexStore -> SQLite/DuckDB` or in‑memory
  * `Cache -> InMemory`
  * others same as above

* **Tests**

  * in‑memory interpreters + fake clock; `Explain` tested via golden files

---

## 7. Dataflows

### 7.1 Search

```
RawQuery -> Parse/Plan -> Index Lookup -> Rank -> Cache Put -> Results
```

Key metrics: `plan_time`, `db_time`, `rank_time`, `cache_hit`.

### 7.2 Indexing

```
Pkg -> Fetch -> Extract -> Tokenize -> Upsert -> Stats
```

---

## 8. Errors & Boundaries

* Unified error type, e.g., `data MoongleError = ParseErr | DbErr | CacheErr | RateLimited | ...`.
* Map backend errors at interpreter boundaries; keep high‑level logic backend‑agnostic.
* Put validation/limits into separate effects (`RateLimit`) and run them *before* heavy work.

---

## 9. Multi‑tenancy & Security

* Carry `TenantId`, `RequestId` via `Reader Tenant` (or an `Env`).
* DB isolation by schema/row; cache keys are prefixed with `tenant:`.
* Admin endpoints require an `Auth` effect; capability bounds make it type‑obvious.

---

## 10. Observability

* **Metrics**: QPS, P95 latencies, cache hit ratio, stage timings, error taxonomy.
* **Tracing**: inject `TraceId/SpanId` at interpreter boundaries; spans per stage.
* **Logging**: structured JSON with `tenant, reqId, planHash, counts`.

---

## 11. Planned Layout

```
/app
  /server                       -- HTTP entry
  /cli                          -- offline tools
/lib
  Moongle/Eff/Search.hs         -- High-level DSL (SearchApp)
  Moongle/Eff/Index.hs          -- High-level DSL (IndexApp)
  Moongle/Eff/Capabilities.hs
  Moongle/Run/Search.hs         -- High-level → capabilities interpreter
  Moongle/Run/Index.hs
  Moongle/Infra/Postgres/IndexStore.hs
  Moongle/Infra/Redis/Cache.hs
  Moongle/Infra/Parser/Moonbit.hs
  Moongle/Infra/Ranker/Default.hs
  Moongle/Infra/Telemetry/Prom.hs
/test
  SearchSpec.hs
  IndexSpec.hs
```

---

## 12. Development Guidelines

* **Qualified imports** for capability modules to avoid name clashes.
* **Newtype everything**: `RawQuery/Plan/DefId/PkgName/TenantId`.
* **Purity first**: keep Parser/Plan/Rank mostly pure; do IO only in interpreters.
* **Minimal constraints** on high‑level functions (`Parser :> es, ...`).
* **Explain first**: define plans and make them inspectable early.

---

## 13. Extension Guide

Adding a new capability/backend:

1. Define the effect interface in `Capabilities`.
2. Provide a default in‑memory interpreter (test first).
3. Implement concrete infra in `Infra/*` (PG/Redis/etc.).
4. Wire it into the runtime; update metrics and docs.

Adding an API:

* HTTP should call high‑level DSLs only:

  * `POST /api/search` → `SearchApp.Resolve`
  * `POST /api/explain` → `SearchApp.Explain`
  * `POST /api/reindex` → `IndexApp.ReindexPackage`

---

## 14. Testing Strategy

* **Unit**: capability in‑memory impls + high‑level interpreters.
* **Golden**: stable `Explain` outputs (text or structured).
* **Properties**: invariants (cache/no‑cache yields same results; stability of ranking).
* **Integration**: a minimal E2E suite against real PG/Redis for migrations/regressions.

---

## 15. Deploy & Run

* **Server**: PG + Redis + Prom/OTel; configuration via env or typed config.
* **CLI**: defaults to in‑memory or SQLite/DuckDB for local search and offline indexing.
* **Config**: `Config { db, cache, metrics, rank, limits }` injected via `Reader`.

---

## 16. Trade‑offs

* **Abstraction cost**: too many layers can add overhead/complexity — profile and inline where needed.
* **Semantic parity**: ensure memory/PG backends behave the same; centralize compliance tests.
* **Plan evolution**: moving from text plans to a structured AST will require Explain/test updates.

---

## 17. Roadmap

* Structured `Plan` (operators + cost model) for richer Explain/optimization.
* Learning‑to‑rank (LTR) and ANN vector retrieval as Ranker options.
* Stage‑level caching and plan caching (plan hashing).
* WASM plug‑ins for parsing/ranking to enable community extensions.

---

## 18. Appendix: Minimal Skeleton

```haskell
-- High-level DSL

data SearchApp :: Effect where
  Resolve :: RawQuery -> SearchApp m [Result]
  Explain :: RawQuery -> SearchApp m Plan

-- Core interpreter
runSearchApp
  :: ( Parser :> es, IndexStore :> es, Ranker :> es
     , Cache :> es, Telemetry :> es )
  => Eff (SearchApp : es) a -> Eff es a

-- Server assembly
runServer :: Env -> IO ()
runServer env =
  runEff
  . runTelemetryProm env
  . runRankerDefault env
  . runCacheRedis env
  . runIndexStorePostgres env
  . runParserMoonbit env
  . runSearchApp
  $ httpApp
```

---

**Maintenance TL;DR**

* High‑level code states *what* to do; interpreters define *how*.
* Add features by extending high‑level DSLs, then wiring or adding capability interpreters.
* Keep **Test + Explain + Observability** as a triad to support safe evolution.

