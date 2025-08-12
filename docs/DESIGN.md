# Moongle TypeSearch GIN Index — Design Spec

---

## 0. Goals

* Fast, composable **type-based search** over MoonBit function signatures.
* Support **partial type-path** queries (e.g., `Result.*`, `Array.JSON.*`).
* Express **generator/processor** queries ("produces T" vs. "consumes T").
* Treat **effects/raises** as first-class filterable items.
* Be **α-equivalence** & **argument-order** robust for polymorphic types.
* Rank by relevance with simple, explainable signals.

Out of scope (v1): learned ranking, fuzzy name matching, result pagination UX.

---

## 1. Storage Model

**Table**: `defs` (one row per exported definition/function)

```text
hank@/tmp:postgres> \d defs;
+-------------+----------+--------------------------------------------------------+
| Column      | Type     | Modifiers                                              |
|-------------+----------+--------------------------------------------------------|
| def_id      | bigint   |  not null default nextval('defs_def_id_seq'::regclass) |
| username    | text     |  not null                                              |
| mod         | text     |  not null                                              |
| pkg_path    | text[]   |  not null                                              |
| pkg_version | text     |  not null                                              |
| fun_name    | text     |  not null                                              |
| pretty_sig  | text     |  not null                                              |
| visibility  | text     |  not null                                              |
| kind        | text     |  not null                                              |
| tokens_lex  | text[]   |  not null                                              |
| tokens      | tsvector |  not null                                              |
| arity       | integer  |  not null                                              |
| has_async   | boolean  |  not null                                              |
| may_raise   | boolean  |  not null                                              |
| version_tag | text     |  not null                                              |
| src_file    | text     |                                                        |
| src_line    | integer  |                                                        |
| src_col     | integer  |                                                        |
+-------------+----------+--------------------------------------------------------+
Indexes:
    "defs_pkey" PRIMARY KEY, btree (def_id)
    "defs_pkg" btree (username, mod, pkg_version)
    "defs_tokens_gin" gin (tokens)
```

> We deliberately keep `tokens` as a prebuilt `tsvector` for speed and control.

---

## 2. Lexeme Schema

Each **lexeme** is an atom stored in `tsvector` with format:

```
<kind>,<occ>,<payload>
```

* **`<kind>`**: short category code

  * `mn` — mention by **name/path** (types, constructors, modules)
  * `mh` — mention by **hash/id** (optional if MoonBit has stable type IDs)
  * `v`  — **type variable** occurrence pattern
  * `me` — **effect/raise** mention
  * `ar` — **arity** (function parameter count) — sorting/filters
* **`<occ>`**: occurrence slot

  * integer `1..n`: appears among value **parameters** (aggregate count)
  * `r`: appears in the **return** position
  * `e`: effect **raised** by the function
* **`<payload>`**:

  * For `mn`/`me`/`mh`: **reversed type path** with trailing dot, e.g. `Result.` or `Option.Result.` or `Array.JSON.data.`
  * For `v`: a **stable type-variable id** (decimal digits), e.g. `1`, `2` (see §3)
  * For `ar`: integer arity, e.g. `2` (no dot)

**Examples**

```
mn,1,List.
mn,1,Array.JSON.
mn,r,Result.
me,e,IO.Error.
v,2,2       -- variable #2 appears twice in params
v,r,2       -- variable #2 appears in return
ar,3        -- 3 parameters
```

> We always **quote and escape** lexemes when building the `tsvector` literal to keep punctuation like `.` or `#` intact.

---

## 3. Normalization Rules (index-time)

1. **Reverse type paths**, append trailing `.`

   * `data.JSON.Array` → `Array.JSON.data.`
   * Rationale: enables prefix (`:*`) queries for **partial path** at any depth.
2. **Case**: store **lowercase** payloads; normalize user queries to lowercase.
3. **Type variables** → numbered ids ensuring α-equivalence & arg-order robustness:

   * Compute multiplicities of each type variable.
   * Sort vars by **descending multiplicity**, then **left-to-right** tie-break.
   * Assign ids `1,2,...` in that order; emit `v,<count>,<id>` and `v,r,<id>`.
4. **Arity**: count value parameters only; emit single `ar,<k>` lexeme.
5. **Effects/Raises**: for each raised effect path, emit `me,e,<revpath>.`.
6. **Traits/Constraints (optional v1.1)**: emit `tc,1,<revpath>.` if needed.

---

## 4. Indexing Pipeline

1. Parse MoonBit signature → normalized AST (already in `signature_jsonb`).
2. Walk AST to collect:

   * Mentioned **concrete types/constructors/paths** with occurrence slots (`1` for params aggregate; `r` for return).
   * **Type variables** with counts and positions (params vs. return).
   * **Effects/raises**.
   * **Arity**.
3. Build lexeme strings; **quote/escape** each; join into a `tsvector` literal.
4. `UPDATE defs SET tokens = '<literal>'::tsvector WHERE id = ...;`

> Rebuild strategy: bump `index_version`, backfill column, swap.

---

## 5. Query Language → SQL (`tsquery`)

We always use `to_tsquery` with quoted lexemes and `:*` for prefix.

### 5.1 Partial path mention (any slot)

```sql
-- any mention of List.*
WHERE tokens @@ to_tsquery('''mn,1,list.*'':*')
   OR tokens @@ to_tsquery('''mn,r,list.*'':*')
```

### 5.2 Generator: **returns T**, but **does not consume T**

```sql
WHERE tokens @@ to_tsquery('''mn,r,result.*'':*')
  AND NOT (tokens @@ to_tsquery('''mn,1,result.*'':*'))
```

### 5.3 Processor: **consumes T**, but **does not return T**

```sql
WHERE tokens @@ to_tsquery('''mn,1,text.*'':*')
  AND NOT (tokens @@ to_tsquery('''mn,r,text.*'':*'))
```

### 5.4 Effect handler / effectful functions

```sql
-- functions that raise IO.Error.*
WHERE tokens @@ to_tsquery('''me,e,io.error.*'':*')
```

### 5.5 Variable-shape queries (α-robust)

* Example: pattern `a -> b -> b` ("returns the second arg’s type")

```sql
WHERE tokens @@ to_tsquery('''v,1,1'' & ''v,2,2'' & ''v,r,2''')
```

* Example: pattern `a -> a` (id function)

```sql
WHERE tokens @@ to_tsquery('''v,1,1'' & ''v,r,1''')
```

### 5.6 Arity filter

```sql
-- exactly 2 params
AND tokens @@ to_tsquery('''ar,2''')
```

> Compose with `&` (AND), `|` (OR), `!` (NOT). Always lowercase user text and reverse paths before building `tsquery`.

---

## 6. Ranking (v1)

Base: `ts_rank_cd(tokens, <tsquery>, weights)` where `weights = '{A,B,C,D}'`.

* Assign heavier weights to **return/effect** lexemes by duplicating them into higher-weighted sub-`tsvector`s if needed; a simpler v1: keep one column and apply **secondary ordering**:

  1. `mn,r,*` hits count desc
  2. `me,e,*` hits count desc
  3. `mn,1,*` hits count desc
  4. `depth` proximity (longer exact path preferred)
  5. `arity` closeness to requested

Add lightweight tie-breakers: popularity (import count), module quality flags.

---

## 7. Escaping Rules

* Always quote each lexeme when constructing `tsvector` and `tsquery`.
* Allowed punctuation in payload (`.` `#` etc.) is preserved by quoting.
* Lowercase payloads; ensure the query builder lowercases user input.

Helpers (pseudo-code):

```haskell
mkLexeme :: Text -> Text -> Text -> Text
mkLexeme kind occ payload = quote (kind <> "," <> occ <> "," <> payload)

revPath :: [Text] -> Text
revPath segs = toLower (Text.intercalate "." (reverse segs)) <> "."

mkPrefix :: Text -> Text
mkPrefix lex = quote lex <> ":*"
```

---

## 8. Examples

Signature:

```
fn[T] read_json(s: String) -> Result[T, Error] raises IO.Error
```

Emitted lexemes (lowercased, illustrative):

```
mn,1,string.
mn,1,error.
mn,1,result.
mn,r,result.
me,e,io.error.
-- type vars
v,1,1         -- T appears once in params
v,r,1         -- T appears in return
-- arity
ar,1
```

Queries possible:

* **Generator of Result**: `mn,r,result.* & ! mn,1,result.*`
* **Effect IO.Error**: `me,e,io.error.*`
* **Returns its param** (`a -> a`): `v,1,1 & v,r,1`

---

## 9. Implementation Plan

1. **Indexer** (Haskell)

   * Parse MoonBit signatures → `signature_jsonb` (already available in project).
   * Walk AST to collect mentions, vars, effects, arity.
   * Normalize → set of lexemes (Text).
   * Build `tsvector` literal and `UPDATE defs SET tokens = ...` in batches.
2. **Search API**

   * Request → parsed filters (paths/effects/patterns/arity).
   * Normalize inputs (lowercase, reverse paths).
   * Compose `tsquery` string(s).
   * `SELECT ... FROM defs WHERE tokens @@ to_tsquery($1) ORDER BY rank ... LIMIT/OFFSET`.
3. **Tests**

   * Golden tests: signature → lexeme set.
   * Property tests: α-equivalent signatures produce identical `v` lexemes.
   * Query fixtures: known queries return expected ids.

---

## 10. Edge Cases & Notes

* **Aliases**: v1 create only canonical mentions; v1.1 may add alias mentions (e.g., `al,*,path.`) and cost them during ranking.
* **Nested generics / tuples / unions**: traverse fully; emit all path mentions.
* **Operators / infix defs**: use the normalized def name; unrelated to type index.
* **Names containing dots**: treat `.` as path delimiter only in type paths; user space names that contain `.` should be escaped at AST level.
* **Multiple effects**: emit multiple `me,e,*` lexemes.
* **No-effect functions**: emit none for `me`.
* **Versioning**: bump `index_version` on schema/rule changes.

---

## 11. Future Extensions

* `tc` for trait/constraint mentions (filter: requires `Eq.*`, etc.).
* `mh` stable type IDs to survive renames.
* Learned ranking features; click logs.
* Per-package shards and parallel GIN build.
* Materialized views for top queries.

---

## 12. Quick Reference (Cheat Sheet)

* **Lexeme**: `<kind>,<occ>,<payload>`
* **Kinds**: `mn` | `mh` | `v` | `me` | `ar`
* **Occ**: `1..n` (params) | `r` (return) | `e` (effect)
* **Payload**: reversed path with trailing `.`; for `v` = var id; for `ar` = int
* **Generator**: `mn,r,X.*  &  ! mn,1,X.*`
* **Processor**: `mn,1,X.*  &  ! mn,r,X.*`
* **Effect**: `me,e,E.*`
* **Var shape**: `a->b->b` → `v,1,1  &  v,2,2  &  v,r,2`
* **Arity**: `ar,k`

---

