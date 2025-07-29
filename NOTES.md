## Query Syntax
```text
async [K, V : Compare] (K, V, name? : String = _, age~ : Int = ..) -> Int raise MyError
```


| Element                        | Syntax                                                                                                                                                                                                                                                                            | Purpose                                                            | Example               |
| ------------------------------ | --------------------------------------------------------------------- | ------------------------------------------------------------------ | --------------------- |
| **Type parameters** (optional) | `[K, V]`  or  `[T : Eq]`                                                                                                                                                                                                                                                          | Declare generics and constraints, mirroring Moonbit’s `fn` header. | `[T : Compare] …`     |
| **Argument list**              | `(A, B, C)` – comma‑separated. If there is **one** argument the parentheses may be omitted.                                                                                                                                                                                       | Shows Moonbit’s non‑curried call style.                            | `(Bytes, StringView)` |
| **Arrow**                      | `->`  *exactly once*                                                                                                                                                                                                                                                              | Separates arguments from the result type.                          | `(K, V) -> Int`       |
| **Return type**                | Any type expression; may end with `raise?` (may throw) or `raise` (always throws).                                                                                                                                                                                                | Encodes error behaviour.                                           | `Array[T] raise?`     |
| **Wildcards**                  | `_`   (matches any single type) <br> `Self[_]` (matches any receiver type)                                                                                                                                                                                                        | Write partial queries when unsure of exact types.                  | `(Array[_]) -> _?`    |
| **Implicit rules**             | • Identifiers **not** found in the “known concrete‑types” set are treated as type variables.<br>• Case is **not** significant: `T`, `t`, `Foo` all legal vars.<br>• If the query string contains `->` it is parsed as a type search; otherwise it falls back to name/text search. | —                                                                  |                       |

### Minimal examples

| Query you type            | Matches functions like                         |
| ------------------------- | ---------------------------------------------- |
| `(K, V) -> Int`           | `fn[K,V] … -> Int`                             |
| `Bytes -> String`         | `fn encodeUtf8(Bytes) -> String`               |
| `[T : Eq] (T, T) -> Bool` | Equality checks requiring `Eq` trait           |
| `_ -> Unit raise?`        | Any single‑argument procedure that *may* raise |

This syntax stays close to real Moonbit signatures, drops the `fn` keyword and parameter names for brevity, and still allows generics, constraints, wildcards, and error markers when you need them.


