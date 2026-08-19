# BYTESXP: vectors of fixed-width opaque data

Prototype of a new SEXPTYPE for values that existing vector types
cannot hold: hashes, UUIDs, IPv6 addresses, 128-bit database keys.
The type is a *storage* type, not a numeric type.  Elements are
compared and hashed as byte blocks and are never interpreted as
numbers, so there is no coercion hierarchy to join and no arithmetic
to define.

Status: **stage 3**.  Everything a filtering workload needs works:
allocation, GC, printing, identity, NA, subsetting, subassignment,
`c()`, `rep()`, comparison, `match`/`unique`/`duplicated`/`split`,
`sort`/`order`/`rank`/`xtfrm`, `as.character`/`format`, and
`table`/`factor`.  Arithmetic, `sum`/`range`, numeric coercion,
`deparse` and serialization are not implemented and fail loudly.

## Decisions

| Decision | Value | Rationale |
| --- | --- | --- |
| Type number | `BYTESXP = 26` | 26-29 free; 30/31 reserved for GC debugging |
| `typeof()` | `"bytes"` | `"data"` reads as data.frame; `"blob"` collides with the CRAN package |
| Element width | gp bits 8-15, 1..255 bytes | Bits 4/5 are `S4_OBJECT_MASK`/`GROWABLE_MASK`. Covers UUID/MD5 (16), SHA-256 (32), SHA-512 (64) |
| `LENGTH` | element count, never bytes | The whole reason this is a new type rather than a strided RAWSXP |
| Ordering | bytewise lexicographic, as stored | Correct for hashes/UUIDs/IPv6, which are conventionally big-endian byte strings. Endianness is an ingest-time concern |
| NA | reserved all-`0xFF` element | See "NA" below |

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

## Files touched

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
| `src/library/base/R/bytes.R` | `bytes`, `as.bytes`, `bytesNA`, `bytesRaw`, `bytesWidth`, `is.bytes` |

Stage 2 added:

| File | Change |
| --- | --- |
| `src/main/subset.c` | `ExtractSubset` (NA branch), matrix subset, `x[[i]]` |
| `src/main/subassign.c` | `case 2626` in all four assign paths, `EnlargeVector`, the type gate |
| `src/main/bind.c` | `BytesAnswer`, exclusive flag 1024, `ans_width` |
| `src/main/relop.c` | `bytes_relop` (memcmp, NA-propagating), dispatched before all coercion |
| `src/main/unique.c` | `byteshash`/`bytesequal`, `HashTableSetup`, `match5` type rule, the unique copy loop |
| `src/main/seq.c` | `rep`, `rep_len`, `rep(each=)` |
| `src/main/builtin.c` | `length<-` |
| `src/main/split-incl.c` | `split` |
| `src/main/coerce.c` | `is.na` checks the sentinel, `anyNA` |

Stage 3 added:

| File | Change |
| --- | --- |
| `src/main/sort.c` | `bcmp_`, `isUnsorted`, `bsort2`, `equal`/`greater`/`listgreater`, both `orderVector1` variants |
| `src/main/coerce.c` | `coerceToString` (hex), and BYTESXP admitted to `coerceVector`'s source gate |
| `src/main/paste.c` | `do_format` |
| `src/library/base/R/format.R` | `format.default` dispatches on `mode()`, so it needs its own arm |
| `src/main/eval.c` | self-evaluating constant, interpreted `for()`, byte-code `STEPFOR` |

`R_allocBytesVector` is declared in `Defn.h`, not `Rinternals.h`:
anything declared in an installed header needs a matching WRE
`@apifun`/`@eapifun` entry or `tools:::checkAPI()` (reg-tests-1e)
fails.  That is deferred until the API is settled.

## The four silent failures found and closed (stage 1)

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

4. **Persistence and boundary** -- `serialize.c`, `deparse.c`, and the
   `.Call` opt-in ported from the int64 FFI-boundary branch.
5. **Tail sweep** -- the remaining sites among the 113 `case RAWSXP:`
   locations in `src/main`: `array.c`, `apply.c`, `split-incl.c`,
   `scan.c`, `connections.c`, plus deliberate refusals in `coerce.c`.
   ALTREP wrappers must refuse BYTESXP (`altclasses.c:1576`) -- on the
   wide-int branch `wrap_meta` hid the wide bit; here a wrapper would
   drop the width.

## Stage 3 notes

Ordering is `memcmp`, so the order is unsigned lexicographic over the
stored bytes.  Verified against a hex-string reference on 300 random
width-6 elements: `order`, `rank`, `sort`, `unique` and `match` all
agree exactly with the same operations on `as.character(x)`.

The specialized `memcmp` comparator in `orderVector1` is a bare
comparison with no NA check, which is only correct because the NA
pre-pass partitions NAs out of `[lo, hi]` first.  Both had to be added
together -- with the comparator alone, `0xFF` bytes sort last
unconditionally and `na.last = FALSE` is silently wrong.  The generic
`greater()` fallback is NA-aware and would have been correct on its
own, just slower.

`table()` and `factor()` turned out to be blocked on `as.character`,
not on ordering.  Both are filtering operations, so hex coercion and
`format()` came in here rather than waiting for a coercion stage.
Note that `format.default` switches on `mode(x)`, not `typeof(x)`, and
needs its own arm in R code -- the C-level `do_format` case is not
reached otherwise.

MSD byte radix (256 buckets, `width` passes, no comparator) is still
unimplemented; the shell sort is correct and was the cheaper thing to
get right first.

Two evaluation paths turned up only when a real workload was tried.
`eval()`'s self-evaluating-constant list did not include BYTESXP, so
anything that evaluates a value -- `do.call()` most visibly -- hit
`UNIMPLEMENTED_TYPE("eval")`.  And both `for()` loops needed their own
case: the byte-code `STEPFOR` reuses a cached length-1 loop variable
via `GET_VEC_LOOP_VALUE`, which both calls `allocVector(TYPEOF(seq),
1)` and would carry a stale width between loops over vectors of
different widths.  BYTESXP allocates a fresh element each iteration
instead.  This is the same shape as the wide-int lesson that byte-code
fast paths must bail out for a new representation.

## A bug worth recording

`ans_width` was added to `struct BindData` for stage 2 and initialized
at the two sites stage 2 touched (`do_c`, `do_unlist`) but not at the
third (`do_bind`, for `cbind`/`rbind`), which reads uninitialized
stack memory.  It surfaced as `cannot combine 'bytes' vectors of
widths 1869528856 and 8`, with a different number each run.

Two things made it survive: the gauntlet only probed `c()` and
`unlist()`, and `do_bind`'s mode ladder had no BYTESXP branch, so a
bytes argument reached code that never expected one.  All three sites
now zero-initialize at declaration (`struct BindData data = { 0 }`) so
a future field cannot be missed the same way, and `cbind`/`rbind` on
bytes vectors error explicitly.  `dim<-` and matrix subsetting do work,
so bytes matrices exist -- cbind/rbind building them is a gap, not a
restriction.

It was found by running a realistic workload, not by probing
operations, which is the same way the `do.call` and `for()` gaps
turned up.

## NA

NA is the all-`0xFF` element.  Reserving a pattern is what every other
atomic type does -- `NA_INTEGER` is `INT_MIN`, taken straight from the
value domain -- and it is what makes `x[match(a, b)]`, `merge()` and
ragged `rbind` behave the way they do for every other type.

`0xFF` rather than `0x00` because zero is the universal "unset" value
and genuinely appears in the data this type is for: the nil UUID is
standardized, and `0.0.0.0` and `::` are the unspecified addresses.
The all-ones counterparts are rare to unused.  Zero-fill (what RAWSXP
does) was rejected outright: it turns a join miss into a value
indistinguishable from a real one, silently, which is the failure
class this design exists to avoid.

`as.bytes()` warns when the reserved pattern arrives from real data
and becomes NA -- the same corruption entering from the other end, and
the only place it can be caught.

The cost is one unrepresentable value.  At width 16 that is nothing;
at width 1 it is 1/256, and at width 4 it costs the IPv4 broadcast
address.  If that ever matters the escape is one more gp bit: a
per-vector "reserves a sentinel" flag, so a vector can decline NA and
hold all 2^(8w) values at the price of erroring on NA-producing
indices.  That is additive later, since existing vectors default to
"has NA".

An out-of-band validity bitmap -- what Arrow does -- was considered and
rejected.  It works for Arrow because Arrow buffers are immutable and
its operations are a small closed set of bulk kernels.  R vectors are
mutated in place through subassignment and their operations are an
open set across 100+ dispatch sites, every one of which would have to
keep payload and mask in sync.  A sentinel gives each site one thing to
copy; a mask gives it two things that can silently disagree.

## Stage 2 notes

The hash family came out as predicted: one FNV-1a over the element's
bytes plus one `memcmp` in `unique.c` lit up `match`, `%in%`,
`unique`, `duplicated` and `split` together, with no per-width code.

Two things needed more work than the sketch suggested:

**`allocVector(TYPEOF(x), n)` is everywhere.**  Nine call sites across
`subassign.c`, `subset.c`, `unique.c`, `array.c`, `builtin.c`,
`seq.c` and `split-incl.c` allocate "another vector like this one" and
cannot know the width.  They now route through `R_allocVectorLike()`.
Any future stage should expect the same: it is the single most common
reason a BYTESXP path fails.

**BYTESXP = 26 sits above STRSXP in the implicit coercion order.**
Three places use the numeric SEXPTYPE ordering as a type hierarchy;
only `unique.c:1391` (`match5`'s common-type rule, `TYPEOF(x) >=
STRSXP -> STRSXP`) is reachable, and without a guard it sends bytes
through `as.character`.  `summary.c:946` is unreachable behind an
earlier `default: goto invalid_type`, and `bind.c:1368`'s `<= INTSXP`
excludes BYTESXP in the safe direction.  Any new SEXPTYPE numbered
above 16 needs this guard regardless of where it is placed.
