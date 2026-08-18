# BYTESXP: vectors of fixed-width opaque data

Prototype of a new SEXPTYPE for values that existing vector types
cannot hold: hashes, UUIDs, IPv6 addresses, 128-bit database keys.
The type is a *storage* type, not a numeric type.  Elements are
compared and hashed as byte blocks and are never interpreted as
numbers, so there is no coercion hierarchy to join and no arithmetic
to define.

Status: **stage 1** (allocation, GC, printing, identity).  Stages 2-5
below are not implemented; everything they cover fails loudly.

## Decisions

| Decision | Value | Rationale |
| --- | --- | --- |
| Type number | `BYTESXP = 26` | 26-29 free; 30/31 reserved for GC debugging |
| `typeof()` | `"bytes"` | `"data"` reads as data.frame; `"blob"` collides with the CRAN package |
| Element width | gp bits 8-15, 1..255 bytes | Bits 4/5 are `S4_OBJECT_MASK`/`GROWABLE_MASK`. Covers UUID/MD5 (16), SHA-256 (32), SHA-512 (64) |
| `LENGTH` | element count, never bytes | The whole reason this is a new type rather than a strided RAWSXP |
| Ordering | bytewise lexicographic, as stored | Correct for hashes/UUIDs/IPv6, which are conventionally big-endian byte strings. Endianness is an ingest-time concern |
| NA | none in stage 1 | See "Open: the NA question" |

## Why a new type rather than a gp bit on RAWSXP

Two reasons, both structural.

**Length semantics.**  `XLENGTH()` on a RAWSXP is a byte count, and
that is baked into every consumer.  A strided RAWSXP forces a choice
between breaking R-level `length()`/`[`/`c()` (if length stays bytes)
and breaking every byte consumer -- `writeBin`, connections,
`serialize`, `memCompress`, `Rcpp::RawVector` (if length becomes
elements).  Both failures are silent.  A new type keeps `LENGTH` =
elements with no ambiguity.

**The typed accessors already type-check.**  `RAW()` errors unless
`TYPEOF(x) == RAWSXP` (`src/main/memory.c:4221`), and likewise for
`INTEGER`/`REAL`/`COMPLEX`/`LOGICAL`.  So for a *new* type every typed
escape hatch closes by construction -- no routing macros, no
per-accessor guards.  Contrast the wide-int prototype
(`feature/wide-int`), where a gp bit on INTSXP meant auditing every
raw-pointer read and where `DATAPTR_OR_NULL` silently handed int64
payloads to int32 loops.

The residual audit surface is the untyped `DATAPTR` family: 19 sites
in `src/main`, of which only `dotcode.c:1917` (`.C` marshalling),
`radixsort.c:1627`/`1721`, and `altclasses.c:1576`/`1582` (the ALTREP
wrapper) are reachable with a BYTESXP.

## The one element operation

Everything the type needs reduces to a byte block at a computed
offset:

```c
#define BYTEVEC_ELT(x, i) (BYTEVEC_DATA(x) + (R_xlen_t)(i) * BYTEVEC_WIDTH(x))
```

plus two primitives over `BYTEVEC_WIDTH(x)` bytes: `memcmp` and a byte
hash.  No per-width specialization.  That single comparator is enough
for `==`, `<`, `order`, `sort`, `match`, `%in%`, `unique`,
`duplicated`, `table`, `factor`, `split` and `identical`, which is
essentially the entire use case.

## Allocation

`R_allocBytesVector(n, width)` allocates a `RAWSXP` of `n * width`
bytes -- so the standard allocator picks the size class and does the
heap accounting -- then retypes it and sets the true element count.
`getVecSizeInVEC()` multiplies the width back out, so the GC sees the
same byte size at allocation and at collection.

`allocVector(BYTESXP, n)` errors: the generic allocator cannot know the
width.  This is why `duplicate.c` needs its own BYTESXP case rather
than `DUPLICATE_ATOMIC_VECTOR`, which calls
`allocVector(TYPEOF(from), n)`.

The width lives in gp, and `PackFlags()` (`src/main/serialize.c:730`)
already encodes gp into the flags word with `UnpackFlags()` running
before allocation on read -- so once stage 4 writes the payload, the
width round-trips through save/load for free.

## Files touched (stage 1)

| File | Change |
| --- | --- |
| `src/include/Rinternals.h` | `BYTESXP = 26` in both the `#define` and enum blocks |
| `src/include/Defn.h` | width accessors, `BYTEVEC_ELT`, entry-point declarations |
| `src/include/Rinlinedfuns.h` | `length`, `xlength`, `isVector`, `isVectorAtomic` |
| `src/main/bytes.c` | new: allocator + `.Internal`s |
| `src/main/memory.c` | `getVecSizeInVEC`, `DO_CHILDREN`, `allocVector3`, `sexptype2char` |
| `src/main/duplicate.c` | `duplicate1` and `lazy_duplicate` |
| `src/main/util.c` | one `TypeTable` row drives `typeof`/`type2str`/`str2type` |
| `src/main/identical.c` | width + payload comparison |
| `src/main/coerce.c` | `is.na`, `is.atomic` |
| `src/main/bind.c` | `AnswerType`: refuse rather than fall through to list |
| `src/main/print*.c` | `EncodeBytes` (hex), `printBytesVectorS` |
| `src/main/inspect.c` | width and payload display |
| `src/main/names.c`, `src/include/Internal.h` | four `.Internal`s |
| `src/main/Makefile.{in,win}` | both list sources; Windows was the wide-int CI failure |
| `src/library/base/R/bytes.R` | `bytes`, `as.bytes`, `bytesRaw`, `bytesWidth`, `is.bytes` |

`R_allocBytesVector` is declared in `Defn.h`, not `Rinternals.h`:
anything declared in an installed header needs a matching WRE
`@apifun`/`@eapifun` entry or `tools:::checkAPI()` (reg-tests-1e)
fails.  That is deferred until the API is settled.

## The four silent failures found and closed

Stage 1's real value was finding where an unhandled type returns a
wrong answer instead of erroring.  All four are in code whose `default`
branch assumes an unknown type is a scalar or a constant:

1. **`length()`/`xlength()`** -- `default: return 1`, so `length(x)`
   silently returned 1 instead of the element count.
   (`Rinlinedfuns.h:566`, `:604`)
2. **`identical()`** -- `default: return TRUE` with a `printf` to
   stdout, so two *different* bytes vectors compared equal.
   (`identical.c:367`)
3. **`c()`/`unlist()`** -- `AnswerType`'s `default` sets the list flag,
   so `c(x, y)` silently returned a list of two.  (`bind.c:148`)
4. **`is.na()`** -- warned "applied to non-(list or vector)" and
   returned all-FALSE.  The value is right for a type with no NA, but
   only by accident.

Everything else failed loudly on the first run: subsetting, `==`,
`sort`, `order`, `match`, `unique`, `table`, `factor`, `rep`, `rev`,
arithmetic, `sum`, all coercions, `format`, `deparse`, `serialize`,
`str`, `lapply`, `split`, matrix printing.

## Remaining stages

2. **Identity and filtering** -- `subset.c` (`ExtractSubset` plus the
   NA branch), `subassign.c` (same width only), `bind.c`,
   `relop.c` (`==`, `!=`), `seq.c` (`rep`), and the byte hash +
   equality in `unique.c` that lights up match/unique/duplicated/
   factor/table together.
3. **Ordering** -- `sort.c` comparator; MSD byte radix is a natural fit
   (256 buckets, `width` passes, no comparator).
4. **Persistence and boundary** -- `serialize.c`, `deparse.c`, and the
   `.Call` opt-in ported from the int64 FFI-boundary branch.
5. **Tail sweep** -- the remaining sites among the 113 `case RAWSXP:`
   locations in `src/main`: `array.c`, `apply.c`, `split-incl.c`,
   `scan.c`, `connections.c`, plus deliberate refusals in `coerce.c`.
   ALTREP wrappers must refuse BYTESXP (`altclasses.c:1576`) -- on the
   wide-int branch `wrap_meta` hid the wide bit; here a wrapper would
   drop the width.

## Open: the NA question

Stage 1 has no NA, which is coherent as long as nothing can
manufacture one.  Stage 2 breaks that: `x[i]` where `i` contains `NA`
or exceeds the length is how R produces NA, and it happens constantly
in exactly the join/filter workflows this type is for -- `x[match(a,
b)]` with no-matches, `merge`, ragged `rbind`, `rep_len`.
`ExtractSubset`'s NA branch must write something.

Three options: error on NA indices (breaks the main use case), fill
with zeros (silently conflates with a real all-zero value), or reserve
a sentinel pattern.  Reserving all-`0xFF` costs nothing real at width
16 but is a genuine restriction at width 1.  This must be decided
before `ExtractSubset` is written, not after.
