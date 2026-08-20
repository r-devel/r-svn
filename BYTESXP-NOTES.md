# BYTESXP: vectors of fixed-width opaque data

Prototype of a new SEXPTYPE for values that existing vector types
cannot hold: hashes, UUIDs, IPv6 addresses, 128-bit database keys.
The type is a *storage* type, not a numeric type.  Elements are
compared and hashed as byte blocks and are never interpreted as
numbers, so there is no coercion hierarchy to join and no arithmetic
to define.

Status: **the original stage list is complete, and so is the ingest
surface**.  Everything a filtering workload needs works: allocation,
GC, printing, identity, NA, subsetting, subassignment, `c()`, `rep()`,
comparison, `match`/`unique`/`duplicated`/`split`,
`sort`/`order`/`rank`/`xtfrm`, `as.character`/`format`, and
`table`/`factor`, serialization, `deparse`, matrices
(`cbind`/`rbind`/`matrix`/`t`/`aperm`/`apply` and matrix printing);
and for the numeric kinds, `+ - * %/% %% /` `^`, unary minus,
`sum`/`prod`/`min`/`max`/`range`, and `as.integer`/`as.numeric`.

Getting data *in* is the second half, and every door is now open: a
package-facing C API in `Rinternals.h` (the one that matters, since
that is how arrow, parquet and database drivers would produce these
vectors), text via `as.bytes(<character>)`, binary files via
`readBin`/`writeBin`, delimited files via `scan()` and
`read.table(colClasses = "int64")`, and `bitwAnd` and friends for the
masking the opaque kind exists to do.  No `.Call` boundary is needed --
see below.  `?bytes` documents the R-level side.

## Decisions

| Decision | Value | Rationale |
| --- | --- | --- |
| Type number | `BYTESXP = 26` | 26-29 free; 30/31 reserved for GC debugging |
| `typeof()` | derived from (kind, width): `"uint64"`, `"int128"`, `"bytes16"` | see "The R-level type name" |
| Element kind | gp bits 0-1: `opaque` / `unsigned` / `signed` |
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

Numeric kinds added:

| File | Change |
| --- | --- |
| `src/include/Defn.h` | kind field, `BYTEVEC_MSB`, kind-aware entry points |
| `src/main/bytes.c` | per-kind NA, `R_bytesEltCmp`, general decimal, `R_bytesEltRender`, `bytesKind` |
| `src/main/sort.c` | `bcmp_` and both `orderVector1` comparators take the kind |
| `src/main/relop.c`, `identical.c`, `unique.c`, `bind.c`, `subassign.c` | kind is part of the type |
| `src/main/coerce.c`, `paste.c`, `printvector.c`, `printutils.c` | render via `R_bytesEltRender` |

Ingest added:

| File | Change |
| --- | --- |
| `src/include/Rinternals.h` | `R_allocBytesVector` and ten accessors: the package-facing API |
| `doc/manual/R-exts.texi` | documents them; `tools:::checkAPI()` fails without this |
| `src/main/memory.c` | one allocator taking width, kind and NA; `BYTES`/`BYTES_RO` |
| `src/main/bytes.c` | text parser, `R_bytesTypeFromName`, bitwise kernels |
| `src/main/bytesarith.c` | `resultFits` becomes the shared `R_bytesMagFits` |
| `src/main/deparse.c` | the constructor call now carries text, not raw bytes |
| `src/main/connections.c` | `readBin`/`writeBin` |
| `src/main/relop.c` | `do_bitwise` dispatch |
| `src/library/base/R/connections.R` | `what` resolution, incl. a pre-existing bug |
| `src/library/base/man/{readBin,bitwise}.Rd` | both document the type |
| `tests/bytes-ffi/` | the opt-in half of the FFI probe |
| `bytesxp-pcheck.R` | text conversion vs Python, self-contained |

Native arithmetic added:

| File | Change |
| --- | --- |
| `src/main/bytesarith.c` | native kernels, hoisted loops, `eltNeg` |
| `src/library/utils/src/size.c` | `object.size()` needs the width |
| `bytesxp-archeck.R` | self-contained; runs both paths and compares |

Tail sweep added:

| File | Change |
| --- | --- |
| `src/main/bytesarith.c` | `R_bytesFromBytes`, `eltConvert`, `as.raw` |
| `src/main/coerce.c` | `as.raw` routing; the mode names keep `na` |
| `src/main/apply.c` | `vapply` over a `bytes` prototype |
| `src/library/base/man/bytes.Rd` | the conversions |

Text readers added:

| File | Change |
| --- | --- |
| `src/main/scan.c` | `scanVector` takes the prototype; `extractItem` |
| `src/main/builtin.c` | `vector()` accepts a `bytes` mode name |
| `src/main/coerce.c` | `as.vector()`, `storage.mode<-` |
| `src/main/bytes.c` | `R_bytesConvert`, shared by all four |
| `src/library/utils/R/readtable.R` | `colClasses` builds a prototype |
| `src/library/utils/R/str.R` | report `typeof()`, not the coarse mode |
| `src/library/base/man/bytes.Rd` | the type's own help page, at last |

Arithmetic added:

| File | Change |
| --- | --- |
| `src/main/bytesarith.c` | new: kernels, `R_bytesArith`, `R_bytesUnary`, `R_bytesCoerce`, `R_bytesSummary` |
| `src/main/arithmetic.c` | `R_binary` and `R_unary` dispatch; `/` and `^` fall through to the double path |
| `src/main/coerce.c` | `as.integer`/`as.numeric` route to `R_bytesCoerce` |
| `src/main/summary.c` | `sum`/`prod`/`min`/`max` divert before the accumulator machinery |
| `src/main/serialize.c` | `WriteItem`/`ReadItem` payload, chunked; `PackFlags` strips GROWABLE |
| `src/main/bytes.c` | `R_bytesSwapWire`, the native <-> canonical mapping |

deparse and matrices added:

| File | Change |
| --- | --- |
| `src/main/deparse.c` | `bytes2buff`, the constructor form |
| `src/main/bind.c` | `cbind`/`rbind`: mode admitted, dim set by hand, block copies |
| `src/main/array.c` | `do_matrix`, `do_transpose`, `aperm` |
| `src/main/subset.c` | `MatrixSubset` |
| `src/main/duplicate.c` | `copyMatrix` (byrow), `copyVector` |
| `src/main/printarray.c` | `printBytesMatrix`, measured column widths |
| `src/main/bytes.c` | block-copy recycle helpers, `R_bytesKindName` |

`R_allocBytesVector` and the accessors are declared in `Rinternals.h`
and documented in WRE -- see "The package-facing API" below.

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

## The tail sweep

Four items, one of which turned out not to be an item at all.

**Converting between `bytes` types.**  `as.vector(int64_vec, "int128")`
now works, along with `as.bytes(x, w, kind)` and `storage.mode(x) <-`
on a vector that is already one.  It is value-preserving in both
directions: widening zero- or sign-extends, narrowing gives `NA` with a
warning for anything that does not fit, and the two numeric kinds
convert into each other by value, so a negative signed value is out of
range for an unsigned target rather than reinterpreted.

The opaque kind takes no part.  Its elements are byte strings, so there
is no value to preserve and no answer to which end of a short one to
pad -- the same question `parseHex()` refuses.  What *is* meaningful for
every kind is changing only the `na` setting, and that is handled
without reading any values: turning the reservation on makes a colliding
value `NA` with the usual warning, turning it off with an `NA` present
is an error.

One design point worth recording: `as.vector(x, "int64")` and
`storage.mode(x) <- "int64"` **keep whatever `na` setting `x` already
had**, rather than defaulting it to `TRUE`.  A mode name can say the
width and the kind but not the reservation, and silently switching a
vector's NA policy because the name could not mention it would be the
wrong way to resolve that.  `as.bytes()` is the spelling that takes
`na` explicitly.

**`as.raw()`.**  Now behaves exactly as it does for an integer vector:
a value outside 0..255, or `NA`, becomes `00` and the whole vector
warns "out-of-range values treated as 0 in coercion to raw".  Routed
through `R_bytesCoerce()` alongside `as.integer` and `as.numeric` so
that the opaque refusal and the NA handling stay in one place.  This is
deliberately *not* `bytesRaw()`: `as.raw()` is element-wise and keeps
the length, while `bytesRaw()` returns the whole payload.

**`vapply()`.**  `FUN.VALUE` is a prototype, so `R_allocVectorLike()`
rather than `allocVector()` -- the same substitution `scan.c` needed,
for the same reason.  The interesting part is the type check: equal
SEXPTYPEs are *not* equal types here, since two `bytes` vectors of
different widths or kinds are as different as an integer and a double,
and the existing `valType != commonType` test would not have noticed.
The mismatch report prints two type names in one message, which is
exactly the site the ring of buffers in `R_bytesTypeNameOf()` was added
for -- it stopped being hypothetical.  A differing `na` setting gets its
own message, since the reservation is part of the type but not part of
its name and the shared message would have printed the same name twice.

**ALTREP wrappers turned out to be fine already.**  `wrap_meta()`
switches on `TYPEOF(x)` and returns `x` unchanged from its `default:`,
so a `bytes` vector is never wrapped and the width can never be
dropped.  Verified rather than assumed -- `.Internal(tryWrap(b))`
returns `b` itself -- and pinned with a test so it stays true.

## Remaining

Nothing known.  The gauntlet's section O still asserts that the
operations which *should* fail do; when one of them starts returning,
that is the signal something new has landed.

## Native arithmetic

The kernels worked a byte at a time.  That is what makes every width
work and what makes them readable in schoolbook form, and it is a poor
way to add two numbers the machine can add in one instruction.
Division was worse: 8*w iterations of bitwise long division per
element.

The widths with a C integer type behind them now dispatch to that type,
and the payoff for storing numeric kinds in *native* byte order -- a
decision taken so that ingest is a memcpy -- turns up again here:
reading an element into a C variable is a load, not a shuffle.

Measured on 2 million elements (macOS/arm64, ms per operation):

| | `+` | `*` | `%/%` | unary `-` |
| --- | --- | --- | --- | --- |
| uint64, before | 29 | 126 | 628 | -- |
| uint64, after | 7.7 | 6.7 | 7.5 | -- |
| int64, after | 8.5 | 7.7 | 11.6 | 3.4 (was 45) |
| uint128, after | 7.9 | 7.5 | 12.6 | -- |

The native kernels are only part of that.  Two things in the *loop*
turned out to cost more than the arithmetic did, and both are worth
recording because neither is specific to this type:

  * **`i % nx` and `i % ny` per element.**  Two integer divisions to
    index two operands.  R has `MOD_ITERATE2` for exactly this -- it
    increments and wraps -- and every other arithmetic loop in R already
    uses it.
  * **promotion done unconditionally.**  Both operands were widened to
    the result width and converted back through MSB-first scratch
    buffers, four byte loops per element.  Promotion is by
    `max(width)`, so at least one operand is *already* the result width
    and usually both are; doing it only when `wx != w` removed almost
    all of it.

A third: `R_bytesEltIsNA()` lives in another translation unit, so it is
a real call, and there are two per element.  The first byte it looks at
settles the answer for all but one value in 256, so testing that byte
at the call site leaves only the rare case to the function.

After all that, `+` on 2M uint64 is 7.7 ms against 2.9 ms for R's
integer `+` on 2M int32.  Per byte moved that is within about 35%,
which is a fair way to read it, since a uint64 vector is twice the
size.  The remaining difference is the per-element overflow branch,
which blocks the vectorisation R's plain loops get; turning `na = FALSE`
on -- which removes the NA tests entirely -- changes the time by 4%, so
the NA tests are not where the rest of it is.  Going further would mean
restructuring the loop, not swapping in more instructions.

**The general kernels are still the definition.**  Where `__int128`
exists they cover every arithmetic width, so the byte-at-a-time path
becomes unreachable in an ordinary session -- and unreachable code is
where bugs settle in.  `R_BYTES_GENERIC_ARITH` forces it, and
`bytesxp-archeck.R` runs both: each against Python's exact integers,
and then the two against each other over ~960,000 results.

That last check was vacuous when first written, which is the part worth
remembering.  `system2(env = "R_BYTES_GENERIC_ARITH=")` sets the
variable to the empty string, and `getenv()` reports an empty variable
as *set*, so the "native" run was the general one and the harness was
comparing the general path with itself and passing.  Two fixes: an
empty value now counts as unset, and the harness asserts that the two
runs differ in speed by more than 2x on a division-heavy workload --
they differ by about 130x, so the margin is enormous and the check can
no longer pass while measuring nothing.

One width does not use the builtin.  `__builtin_mul_overflow` on a
signed type the target cannot multiply in one go becomes a call to
compiler-rt's `__mulo?i4`; clang 19 on aarch64 does that for 128 bits,
and a clang configured against libgcc -- Debian's default -- has no
such symbol, so `libR.so` linked and `R.bin` did not.  The signed
128-bit body checks the magnitudes itself instead, one division where
the general kernel would do 256 byte multiplies.  Unsigned keeps the
builtin: compiler-rt has no unsigned counterpart to call.  That the
break showed up on exactly one of twelve CI jobs -- the newer clang
expands it inline, and so does the x86_64 backend -- is the useful part
to remember about builtins that lower to a runtime library.

Writing the check by hand also showed the oracle up.  Its operands were
drawn at the range edges, which hits overflow hard but means that at
width 16 every product that *fit* had a 0 or a +-1 in it: the boundary
the check exists for was never crossed from below.  It now also draws
magnitudes whose bit widths add up to about the width, which puts about
90 of 400 pairs per combination on large products that fit.

Two smaller things surfaced while measuring.  `object.size()` reported
"unimplemented type 'bytes'": `xlength()` counts elements, so anything
wanting a byte count has to ask for the width -- and `utils/src/size.c`
is compiled like package code, so it reaches it through the public
`R_bytesWidth()`, which is a small vindication of that API existing.
And `sum()` over a vector that overflows immediately returns in no time
at all, because it stops at the first overflow; that is correct, and it
made the first benchmark look implausibly good until the data was
changed to something that does not overflow.

## The text readers

`readBin` covers a flat binary file and the C API covers a package
reader, but the form most 64-bit keys actually arrive in is a CSV.
Reading such a column as `character` and converting afterwards costs an
interned `CHARSXP` per row, so `scan()` reads it directly:

```r
scan(f, what = bytes(0L, 8L, "signed"))            # a whole file
scan(f, what = list(id = bytes(0L, 8L, "unsigned"), x = 0), sep = ",")
read.csv(f, colClasses = c(id = "int64"))
```

`scan.c` needed exactly the change the notes above predict for any new
SEXPTYPE, and nothing else.  `scanVector()` took a bare `SEXPTYPE`,
which cannot carry a width, a kind or the NA reservation; it now takes
the prototype, and the four "another vector like this one" sites --
the initial allocation, the doubling, the zero-length return and the
final truncation -- route through `R_allocVectorLike()`.  `extractItem`
gains a case, and `fillBuffer` needed nothing at all: it consults the
type only to decide whether quoting applies, and a `bytes` field is a
plain field exactly as an `integer` one is.

Parse failures **error** rather than giving NA with a warning, unlike
`as.bytes()`.  That is deliberate: `Strtoi` returns `NA_INTEGER` for a
value out of `int` range as well as for one that is not a number, so an
`integer` column already errors on both, and `scan` should not have two
conventions.

**`read.table` recognizes the class by trying to build it.**  The set of
names is open-ended -- any width, three kinds -- so it cannot be a
list, and the earlier guess that `methods::as` would do the job was
wrong: `as()` is S4 machinery needing a registered class, and 255
widths x 3 kinds cannot be registered.  Instead `vector()` learned the
`bytes` mode names, and `read.table` asks it for a prototype and checks
with `is.bytes()` what came back.  That keeps the naming rule in one
place (`R_bytesTypeFromName` in C) and is self-limiting: `vector("list",
0)` builds fine but is not a `bytes` vector, so it is not accepted.

Teaching `vector()` the names meant teaching the mode family, since
half of it would have been worse than none: `as.vector(x, "int64")` and
`storage.mode(x) <- "int64"` work too, both through one shared
`R_bytesConvert()` that also backs `as.bytes()`.

`str()` had to be fixed in the same breath.  `mode()` is deliberately
coarse -- "numeric" for every width and both signs -- so `str(df)`
reported a freshly-read 64-bit column as `num`, which is precisely the
confusion the type exists to prevent, at precisely the moment someone
checks what they just read.  It now reports `typeof()`.

A latent bug surfaced while auditing this: `R_typeToChar()` reports a
`bytes` vector through `R_bytesTypeName()`, which returned a *single*
static buffer, and five of R's messages print two type names in one
call ("incompatible types (from %s to %s)", vapply's mismatch report).
None is reachable with two `bytes` operands today -- the subassign
paths have their own kind/width messages -- but it would have fired
silently, printing one name twice, the first time a new site did.
Fixed with a small ring of buffers, which removes the class rather than
the instance.

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

Byte radix sort landed later; see below.

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

## Arithmetic

Defined for the `unsigned` and `signed` kinds at widths 1, 2, 4, 8 and
16; wider elements stay pure storage and error.  Binary operands
promote to `max(width)` -- widths are totally ordered, so this is a far
simpler lattice than R's usual one -- and kinds never mix.  Overflow,
underflow and division by zero all yield NA with a warning, matching
integer overflow.  `/` and `^` yield a double, as they do for integers.

`%/%` is floor division and `%%` is the matching modulo, so the
remainder takes the sign of the divisor, as for integers.

Every kernel works on a scratch copy held most-significant-byte first
(`src/main/bytesarith.c`).  That costs a copy per element but lets the
algorithms read in the usual schoolbook form, which for arithmetic that
has to be exactly right at 128 bits is the trade worth making.
Division is bitwise long division -- 8*width iterations, cheap at these
widths, and it avoids a normalization step that is easy to get subtly
wrong.  Widths with a native C type behind them could be specialized
later.

A result that lands exactly on the reserved NA value is reported as
overflow rather than silently becoming NA.

`sum`/`prod`/`min`/`max` are handled in a self-contained pass ahead of
`do_summary`'s accumulator machinery rather than inside it: that
machine keeps typed accumulators chosen from a fixed set, and a
per-vector element width does not fit it.  `range` falls out of
`min`/`max` via `range.default`.

`as.integer` gives NA outside integer range, with a warning;
`as.numeric` warns above 2^53, where a double stops being able to name
every integer.  Both refuse `opaque` vectors.

Validated against Python's exact integer arithmetic over 3000 operand
pairs across six width/kind combinations (`bytesxp-archeck.R`), with
the operands deliberately weighted toward the range edges so that
overflow is exercised hard: `+ - * %/% %%` and unary minus all agree,
including every overflow case.

## Serialization

Width and kind live in gp, and `PackFlags` already encodes gp into the
flags word with `UnpackFlags` decoding it before allocation, so they
cost nothing to carry and are available exactly when the reader needs
them to size the vector.  Only the payload needed writing.

The payload is normalized: numeric elements go on the wire **most
significant byte first**, matching what R already does for integers
and reals under XDR, so a file written on one platform reads as the
same values on another.  Opaque elements are byte strings and travel
verbatim, which means that on a big-endian machine the conversion is a
plain copy in every case.  One function serves both directions, since
the mapping is its own inverse.

Verified rather than assumed: serializing the `uint64` value 1 gives
native storage `01 00 00 00 00 00 00 00` and wire bytes
`00 00 00 00 00 00 00 01`, and decoding that payload independently in
Python as a big-endian integer yields 1 (and -5 for the signed case).
An opaque element's wire bytes equal `bytesRaw()` exactly.

No format version bump: a new SEXPTYPE is not a structural change, and
an older R reading one of these files already fails loudly with
"ReadItem: unknown type 26, perhaps written by later version of R".

## Radix sort

Measured before optimizing: sorting 2e5 `uint64` elements took 49ms
against 3ms for the same number of doubles, and `order` 32ms against
3ms -- doubles get a radix sort, `bytes` was on the shell sort.  A
10-16x gap on the core operation of a filtering workload was worth
closing.

LSD rather than MSD: one stable counting-sort pass per byte, least
significant first, `O(width * n)` with no comparisons.  It is simpler
than a recursive MSD and stability falls out, which matters because
R's comparison path breaks ties by index and the radix has to agree.
The signed kind biases the most significant byte by `0x80` so
negatives sort first; a decreasing sort complements every key, which
reverses the order while *keeping* the ascending index tiebreak that
R gives.

Opaque elements stay on the comparison sort: their order is
lexicographic, which `memcmp` gives directly, and the radix would only
add a copy.  Long vectors (> 2^31) also stay on the shell sort.

After: sort 4ms, order 3ms -- level with doubles.

Verified against Python with `bytesxp-rxcheck.R`, deliberately drawing
from a small pool so that ties are everywhere, since ties are what
stability bugs hide in: 48 checks over six width/kind combinations
covering ascending and decreasing order, `sort` agreeing with
`order`, and NA placement on both sides.

## The FFI boundary needs no opt-in

The int64 FFI-boundary branch (r-svn PR #301) narrows INT64SXP
arguments to INTSXP at `.Call` unless the package opts in, so that
unmodified packages keep working.  That rationale does not transfer:
there is no meaningful narrowing of an opaque blob, or of a `uint64`
above `INT_MAX`, so there is nothing to hand an unmodified package
instead.

What remains is whether package code fails *safely*, and that is the
claim the whole design rests on -- so it is tested rather than argued.
`tests/bytes-ffi/` builds a stand-in package with `R CMD SHLIB` whose
functions take the shapes real package C code takes, and hands each a
`uint64` vector:

| pattern | result |
| --- | --- |
| `switch (TYPEOF(x))` with a `default:` | its own error, at the top |
| `REAL(x)[0]` with no check | `REAL() can only be applied to a 'numeric', not 'uint64'` |
| `if (isInteger(x))` | FALSE, so the safe branch is taken |
| moves the SEXP without reading it | works, as it should |
| `XLENGTH(x)` | works, as it should |
| `DATAPTR_RO(x)` | `cannot get data pointer of 'uint64' objects` |

Every read path fails loudly and names the type; the two paths that
*should* work do.  The untyped `DATAPTR` escape hatch -- the one
residual risk identified back in stage 1 -- turns out to be guarded
already by `CHKVEC` in `Rinlinedfuns.h`.

That guard is why `BYTEVEC_DATA()` does *not* go through `DATAPTR()`.
The obvious way to make R's own code work is to add BYTESXP to
`CHKVEC`, and it is the wrong way round: the same `CHKVEC` is what the
out-of-line `DATAPTR_RO` compiled in `inlined.c` runs for package code,
so teaching it about BYTESXP would hand every package a raw pointer to
elements whose width and kind it does not know, and the row above would
turn from `errors` into `returns`.  `BYTEVEC_DATA()` reads
`STDVEC_DATAPTR()` directly, behind a `CHKBYTEVEC()` of its own that
fires under the same build flags.  The two checks are now independent,
which is what lets one say yes and the other no.

One real defect surfaced here: `nvec[]` in `memory.c`, the table that
says which SEXPTYPEs are vectors for the package-facing `LENGTH()`,
had BYTESXP marked as a non-vector, so `XLENGTH(x)` from package code
errored.  R's own code uses the unchecked macro and so never noticed.
Fixed; it is exactly the kind of entry a new SEXPTYPE has to remember.

## The package-facing API

Failing safely is only half of it.  That section is about a package
that has never heard of the type; this one is about a package that
*wants* it.  Nearly every 64-bit integer an R user meets arrives
through one -- arrow, nanoparquet, duckdb, a database driver, a JSON
reader -- and until now none of them could construct a `bytes` vector
at all: only `BYTESXP` itself was in `Rinternals.h`, and every
constructor and accessor was internal.

`Rinternals.h` now declares eleven entry points, and
`doc/manual/R-exts.texi` documents them at the end of "Vector accessor
functions".  Both halves are mandatory: `tools:::checkAPI()`, run from
reg-tests-1e, fails if anything declared in an installed header lacks
a matching `@apifun` line in WRE.  Worth knowing before adding a
declaration -- the failure comes from a regression test, some distance
from the header that caused it.

    SEXP     R_allocBytesVector(R_xlen_t n, int width, int kind,
                                Rboolean hasNA);
    Rboolean R_isBytes(SEXP x);
    int      R_bytesWidth(SEXP x);
    int      R_bytesKind(SEXP x);
    Rboolean R_bytesHasNA(SEXP x);
    Rbyte       *BYTES(SEXP x);
    const Rbyte *BYTES_RO(SEXP x);
    Rbyte       *R_bytesElt(SEXP x, R_xlen_t i);
    const Rbyte *R_bytesEltRO(SEXP x, R_xlen_t i);
    Rboolean R_bytesIsNA(SEXP x, R_xlen_t i);
    void     R_bytesSetNA(SEXP x, R_xlen_t i);

`BYTES`/`BYTES_RO` follow `RAW`/`RAW_RO` deliberately: the typed
accessor is the door, and it type-checks, which is the same property
that makes the untyped `DATAPTR_RO` able to say no.  `BYTES_OPAQUE`,
`BYTES_UNSIGNED` and `BYTES_SIGNED` are the public spellings of the
kind, and `Defn.h` now *defines* `BYTEVEC_OPAQUE` and friends from
them, so the internal and external names cannot drift apart.

Taking all three per-vector properties as arguments collapsed the two
allocators into one and removed `R_bytesWithNA`.  That is not tidying.
The recurring bug on this branch has been a property forgotten at an
allocation site -- the width in `do_bind`, then the kind in
`duplicate1`, `ExtractSubset` and both `for` loops -- and a signature
that cannot be called without naming all three is the one form of the
fix that does not rely on remembering.

`tests/bytes-ffi/` grew a second half that uses only these
declarations: build a `uint64` column with `memcpy` into
`R_bytesElt()`, mark a null with `R_bytesSetNA()`, read the width,
kind and NA-ness back, sum the payload while skipping NA with
`R_bytesIsNA()`, and confirm that `R_bytesWidth()` and `BYTES_RO()`
refuse an integer vector the way `RAW()` does.

## Text is the other ingest route

A raw payload is the fast way in, but it is not the common one.  Nearly
every 64-bit identifier an R user meets arrives as *text* -- a column of
a CSV, a field of a JSON document, a line of a log -- and text is the
only form that carries no byte order for the reader to get wrong.  Until
now the conversion ran one way only: `as.character(x)` gave
`"578437695752307201"` and feeding that back errored.

`as.bytes()` now accepts, besides raw:

  * **character** -- decimal for the numeric kinds, hex for opaque,
    the exact inverse of `as.character()`.  `NA_character_` is NA.
  * **integer** and **logical** -- narrowed, exactly as they are in
    arithmetic, so the same values are admitted by the same rule in
    both places.
  * **double** -- refused, for the reason the whole lattice refuses it.
    The message does *not* offer `as.character()` as the way out: a
    double has already lost the digits by the time it could be printed,
    so that advice would be wrong.

The parser is the mirror of `R_bytesEltDecimal()`: repeated
multiply-by-ten-and-add on an MSB-first accumulator, so it works at
every width rather than only the ones with a native C type.  It sits
next to the renderer in `bytes.c` for that reason, with
`BYTEVEC_MAX_WIDTH` buffers -- text conversion is defined wherever
`as.character()` is, while arithmetic stops at 16 bytes.

The one thing it borrows from `bytesarith.c` is `R_bytesMagFits()`
(formerly the static `resultFits`).  Which values a width admits is the
subtle part -- the reserved NA pattern is a hole in the middle of the
range and `na = FALSE` moves it -- and it is where the review pass
already found one bug.  Two copies of that rule would eventually
disagree.

**Two failures, two warnings**, as `as.integer()` has: `"abc"` gives
"NAs introduced by coercion", `"9223372036854775808"` gives "NAs
introduced by values outside the range of 'int64'".  They are different
mistakes.  Under `na = FALSE` both become errors, since there is no NA
to produce.

A subtlety worth recording: the "equal to the reserved NA value" warning
has to be counted *at parse time*, not by scanning the finished vector.
The first version scanned, and so reported every deliberately-set NA --
including `NA_character_` input -- as a datum that had collided with the
reserved pattern.  Only a successfully parsed element can collide, and
only for the opaque kind, since the numeric parsers report that value as
out of range instead.

Verified against Python's exact integers rather than against itself:
`bytesxp-pcheck.R` (self-contained -- it generates its own reference)
checks 1992 values over eight width/kind combinations, weighted to the
range edges, comparing both the text and the stored native-order bytes.
The gauntlet's round-trip test would pass with two mirrored bugs; this
would not.

## Bitwise operations

The gap this closes is the opposite shape from the others.  Arithmetic
is defined for the numeric kinds and deliberately not for the opaque
one -- there is no number in a UUID to add.  But masking one is an
everyday operation: an IPv6 prefix, a hash bucket, a flag word.  Those
are exactly the operations `bitwAnd` and friends name, and they were
erroring.

They are also the cheapest thing this type can do, more so than
arithmetic was.  `and`, `or`, `xor` and `not` are per byte with no
carry, so there is **no width restriction at all** -- a width-32 SHA-256
mask works, where arithmetic stops at 16.  Only the shifts need to know
which end is significant, and that is one macro:

```c
/* an opaque element is a byte string, so its first stored byte is its
   most significant one on every platform; the numeric kinds are stored
   natively */
#define BITMSB(i, w, k) ((k) == BYTEVEC_OPAQUE ? (i) : BYTEVEC_MSB(i, w))
```

which is why this lives in `bytes.c` and not `bytesarith.c`: it is a
byte-level operation defined at every width, not a value operation
limited to the arithmetic ones.

Rules, all borrowed rather than invented:

  * widths and kinds must match; they are *not* promoted the way
    arithmetic promotes them.  A mask that is not the width of what it
    masks is a mistake, not a value to sign-extend.
  * an integer operand narrows *by value* for the numeric kinds, as in
    arithmetic; the opaque kind refuses it, as everywhere else.  This
    is the one place these differ from R's integer versions, where a
    negative operand is just a bit pattern: `bitwAnd(x, -1L)` is the
    identity for a signed vector, where -1 is a value it holds, and is
    out of range for an unsigned one, where zero- and sign-extension
    would disagree about what it meant.  Refusing keeps both readings
    reachable, which is the same argument that keeps `double` out of
    the lattice.
  * `bitwShiftR` is a logical shift on the bit pattern, which is what
    R's integer version already does (`bitwShiftR(-1L, 1L)` is
    `2147483647`).  A shift of `8 * w` or more is `NA`, mirroring
    R's `> 31`.
  * a result landing on the reserved NA value is reported, not returned
    quietly -- the rule arithmetic follows for overflow.
    `bitwNot(as.bytes("0", 8L, "unsigned"))` is `UINT64_MAX`, which a
    vector reserving NA cannot hold; with `na = FALSE` it just works.

## readBin and writeBin

The sharpest argument for the type is not a hypothetical.  `readBin` has
had a documented way to read a 64-bit integer from a file since forever,
and it silently gives the wrong number:

```r
z <- as.raw(c(1, 0, 0, 0, 1, 0, 0, 0))   # 2^32 + 1, little-endian
readBin(z, "integer", 1, size = 8)       # 1        <- no warning
readBin(z, "int64",   1)                 # 4294967297
```

`(int) u.ll` in `do_readbin` truncates and nothing checks.  That is one
line of the case for the type, on a function every R user knows.

Two spellings, and both were already in `readBin`'s contract.  `what`
resolves anything that is not a known mode name through `typeof()`
(`connections.R:271`), and `typeof()` was already made to return
`"int64"` / `"uint64"` / `"bytes16"`, so:

```r
readBin(con, "int64", n)                            # the name
readBin(con, bytes(0L, 8L, "signed", na = FALSE), n) # the prototype
```

The prototype form is the better one and is passed through whole rather
than reduced to its name, because it carries the *third* property: `na`.
Ingesting a foreign column where every bit pattern is a legitimate value
is exactly the case `na = FALSE` exists for, and a type name cannot say
it.

**What this buys over `as.bytes(readBin(con, "raw", n * w), w, kind)`.**
Mostly one thing, and it is not convenience: **`endian`**.  `as.bytes()`
takes its input verbatim -- that is what makes ingest a memcpy -- so the
raw route can only produce native byte order, and big-endian sources are
everywhere (network protocols, JVM output, PostgreSQL binary COPY, a
good deal of scientific data).  Doing it in R means reversing every
`w`-byte group through a matrix transpose.  In `do_readbin` it is the
swap loop that was already there, `swapb(p + i * size, size)`, with
`size` set to the width.  Secondarily: one buffer instead of two, which
is 2x peak memory on a large file, since `bytesRaw()` copies as well.

The implementation is small because it lands exactly on the existing
shape.  Allocate, point `p` at the payload, set `size = width`, and the
block-read loop, the swap loop, the `signed = FALSE` warning and the
`xlengthgets` truncation for a short read all work unchanged.  Reads
truncate to whole elements: `rawRead` divides by `size`, and
`con->read` counts in units of it.

Two guard rails, both following the function's own idioms:

  * `endian` is refused for the opaque kind, with a warning, the way
    `signed = FALSE` is refused where it does not apply.  A byte string
    has no byte order, and silently reversing a UUID because the file
    came from another machine would be corruption.
  * a payload that arrives equal to the reserved value warns, exactly as
    `as.bytes()` on raw does -- and through the same function, since it
    is the same event.

**A pre-existing bug fixed on the way.**  `readBin`'s fallthrough is
`what <- typeof(what)`, and `typeof()` of an unrecognized *string* is
`"character"`.  So `readBin(z, "int64", 2)` did not error, it read
null-terminated strings -- and so did `readBin(z, "typo", 2)`, on any
version of R.  The condition now only takes `typeof()` when `what` is
not a length-one character string, which matches what the help page has
always said `what` means.  `readBin(con, character())` and the other
prototype forms are unaffected; all 22 regression files still pass.

`writeBin` had to come with it.  It errored loudly before, which is
correct behaviour but makes the type a one-way street, and `endian`
matters more on the write side, where the point is to produce a file
some other system will read.

## deparse and matrices

There is no literal syntax for a `bytes` vector, so `deparse` emits
the call that rebuilds it -- exactly as raw vectors deparse to
`as.raw(c(...))`.  Empty vectors deparse to `bytes(0L, w, kind)`.

Elements go out as the text `as.character()` gives:
`as.bytes(c("1", NA_character_, "-9223372036854775807"), 8L, "signed")`.
Writing the raw payload instead, which is what this did first, was
shorter to produce and much worse in three ways -- eight hex bytes per
element instead of one readable number, output that *differed between a
little- and a big-endian machine* because the payload is stored
natively, and an NA element that came back correct but warned, since a
reserved bit pattern arriving as data is exactly what `as.bytes` is
supposed to flag.  `NA_character_` is unambiguous, so that wart is gone
rather than documented.

Matrices needed more than `cbind`/`rbind`.  `allocMatrix` cannot carry
a per-vector width, so `do_bind` and `do_matrix` build the vector and
set `dim` themselves.  The generic recycle helpers
(`xcopyRawWithRecycle`, `xfillRawMatrixWithRecycle`) assign elements,
which cannot work at a per-vector element size, so `bytes.c` grows
block-copy analogues that reuse `FILL_MATRIX_ITERATE`.  Beyond that,
`MatrixSubset`, `do_transpose`, `aperm`, `copyMatrix`, `copyVector`
and `printMatrix` each needed a case -- all of them mechanical, and
all of them found by simply trying `print(cbind(a, b))` and following
the errors.

`matrix(x, nrow, ncol, byrow = TRUE)` recycles exactly as it does for
integers, which the gauntlet pins against the integer element pattern
rather than against a hand-written expectation.

## A third bug found by use rather than by probing

`match5` has a scalar fast path for length-1 needles with its own type
switch (`src/main/unique.c`).  It had no BYTESXP case, so it fell
through with the result still set to `nomatch`: `match(x[2], x)`
returned NA and `x[2] %in% x` returned FALSE for a value that was
plainly in the table.  Every gauntlet match test used length-2 vectors,
so nothing reached it; it surfaced the first time a realistic join was
written with a single key.

That is now three separate gaps -- `do.call`, both `for()` loops, the
`cbind` width, and this -- that systematic probing missed and running
a real workload found immediately.  The probe suite is good at "does
this fail loudly" and blind to "is this path reachable at all".

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

## The R-level type name

`typeof()` reports a name derived from `(kind, width)` rather than the
SEXPTYPE's own name:

| kind | width | `typeof()` | `mode()` |
| --- | --- | --- | --- |
| unsigned | 8 | `"uint64"` | `"numeric"` |
| signed | 8 | `"int64"` | `"numeric"` |
| signed | 16 | `"int128"` | `"numeric"` |
| unsigned | 4 | `"uint32"` | `"numeric"` |
| opaque | 16 | `"bytes16"` | `"bytes"` |

R already does exactly this for OBJSXP, which reports `"S4"` or
`"object"` for the same SEXPTYPE depending on a gp bit
(`R_typeToChar`, `src/main/util.c:351`).

The point is that package code can dispatch on what it is actually
holding -- `switch(typeof(x), uint64 = ...)` -- which is the main thing
a separate SEXPTYPE otherwise costs you, *without* the type number
lying to C code.  That is the whole argument for not extending INTSXP
instead: there are 1586 `INTEGER(` call sites in R's own `src/` alone,
and a width-customized INTSXP would hand every one of them a pointer
to read as int32.  Here `TYPEOF(x) == INTSXP` is simply false, and
`is.integer()` stays honest.

`mode()` coarsens, so every opaque width shares the mode `"bytes"` and
every numeric kind is `"numeric"`.  Note `mode()` is `"numeric"` while
`is.numeric()` is FALSE -- the same combination factors have, and for
the same reason: the storage is numeric but the object should not be
fed to generic numeric code.

`is.bytes()` asks the C level rather than testing `typeof()`, since
there is no longer a single string to test for.  `format.default`
likewise checks `is.bytes(x)` before its `switch(mode(x), ...)`.

## The coercion lattice: a deliberate partial answer

R's lattice is currently value-preserving: `logical` in `integer` in
`double` in `complex`, and every promotion is exact, since all int32
fit in a double's 53-bit mantissa.  A 64-bit integer would be the first
type to break that -- neither it nor `double` subsumes the other.  That
is *why* bit64 lives outside the lattice, not an oversight.

Every other ecosystem resolves it the same way (int64 + float -> float:
Python, Julia, SQL, Arrow, Polars) and accepts the degradation.  That
rule is materially more dangerous in R, because **`1` is a double
here** while it is an integer in Julia and Python -- so the form users
type by default is the lossy one.  On the INT64SXP branch:

```
x            =  4611686018427387904
x + 1L       =  4611686018427387905    typeof: int64
x + 1        =  4611686018427387904    typeof: double   <- wrong, silently
```

One character separates a correct answer from a wrong one, with no
warning.

So this takes the conservative position, chosen because **an operation
that errors today can start working later without breaking any code
written in the meantime, while the reverse is a breaking change**:

| operands | result |
| --- | --- |
| bytes with bytes | same kind, `max(width)` |
| bytes with logical or integer | narrows in; result is bytes |
| a value that does not fit | NA with a warning, as integer overflow does |
| bytes with double | **error**, naming both fixes |
| `/` and `^` | unchanged -- explicitly double-producing |
| opaque with anything | error; opaque elements have no numeric reading |

The integer promotion is the one every system agrees on and is
lossless, so it costs no policy.  The double case is the entire
controversy, and refusing it keeps both candidate rules -- widen to
double, or narrow into bytes with NA -- reachable later.

The error says what to do:

```
'bytes' and 'double' cannot be combined; use an integer operand (1L),
or as.numeric() for double arithmetic
```

`is.numeric()` stays FALSE, matching the factor precedent; that is a
wide-blast-radius commitment not worth spending yet.

## Declining to reserve a value: `na = FALSE`

At width 16 giving up one value costs nothing; at width 1 it costs
1/256, and at width 4 unsigned it costs the IPv4 broadcast address.
So the reservation is per-vector, in gp bit 2.

The sense is inverted deliberately -- the bit *set* means "no NA", so
the default and anything read from an older file behave as before.

```r
w1 <- as.bytes(as.raw(c(0, 1, 254, 255)), 1L, "unsigned", na = FALSE)
w1                       # 0 1 254 255  -- all 256 patterns are values
max(w1)                  # 255
w1[99]
#> Error: missing values are not representable in this 'uint8' vector;
#>        it was created with na = FALSE
```

A vector that declines the reservation cannot represent a missing
value, so **every operation that would produce one errors instead**:
out-of-range and NA subscripts, `length<-` growth, join misses,
arithmetic overflow, an unrepresentable operand, and `matrix()`'s NA
fill.  The check sits in the NA branch itself, which runs only when an
NA is actually needed, so it costs nothing on the ordinary path.
`is.na()` and `anyNA()` short-circuit to FALSE, and `as.bytes()` does
not warn about the pattern, because nothing is reserved to collide
with.

The flag is part of the type, as the kind and the width are:
combining vectors that disagree would either lose a real value or
invent a missing one, so `c()`, comparison, subassignment and
`identical()` all refuse it.

Two things this shook out, both the same shape: every place that asks
`R_bytesEltIsNA` must first ask whether the vector reserves anything.
`sum`/`min`/`max` read a real `255` as missing, and `deparse` dropped
the flag so a round-trip silently changed the type.  Worth remembering
that adding a per-vector property means auditing not just the
allocator -- which by now is well guarded -- but every *predicate* over
the payload.

## The implicit class vector

`class(x)` returns `c("uint64", "bytes")` -- the shape `class(m)` uses
for `c("matrix", "array")`.  This is the actual path forward, and the
cheapest item here: a package can write `mean.bytes` **once** rather
than one method per width, and prototype the contested semantics in S3
without the core committing to anything.

```r
mean.bytes <- function(x, ...) sum(x) / length(x)
mean(uint64_vector)   # 4
mean(int128_vector)   # 10   -- same method, different width
```

Someone who wants `x + 1` to give a double can have it in their own
package today, which is how the lattice question gets answered by
usage rather than decided upfront.

## Element kinds

Width alone cannot say what the bytes mean: a width-16 UUID and a
width-16 `uint128` are the same size and must sort differently.  So
gp bits 0-1 carry a kind, and there are exactly three:

| Kind | Storage | Order | Display | NA |
| --- | --- | --- | --- | --- |
| `opaque` | verbatim | lexicographic | hex | all-`0xFF` |
| `unsigned` | native byte order | by value | decimal | `UINT_MAX` |
| `signed` | native byte order | by value | decimal | `INT_MIN` |

`opaque` is the default and is what hashes, UUIDs and addresses want.

The numeric kinds store bytes in **native** order, which is the whole
point for the ingest use case: reading an `int64` column out of
Parquet, Arrow or a database driver is a plain `memcpy` with no
transform, and `bytesRaw()` hands the same bytes back for writing out.
Order is by *value* rather than by bytes, so sorting gives the same
answer on every platform even though the storage does not.  (Endianness
is deliberately not a header bit: two representations of one value
would break the single-canonical-encoding property that makes `match`
and hashing free.  Serialization normalizes instead -- see below.)

`signed` cannot use the all-`0xFF` NA, since that is -1 in two's
complement, so it reserves `INT_MIN` -- which is exactly what bit64
does, and for the same reason.

Kinds do not mix.  `c()`, comparison, subassignment, `identical` and
`match` all treat a differing kind the way they treat a differing
width: as a different type.

Decimal rendering is general rather than limited to widths with a
native C type behind them: repeated division by 10 over a scratch
copy, so a 128-bit or 96-bit value prints exactly.  Verified against
Python's arbitrary-precision integers over 1618 random values across
four width/kind combinations (`bytesxp-xcheck.R`), including the
extremes of each range -- decimal text, `order`, `sort`, `rank`,
`unique`, `match` and byte-for-byte round-trip all agree.

## A second kind-dropping bug, same shape as the first

`R_allocBytesVector(n, width)` predates the kind field, so every
internal "another vector like this one" site that called it silently
produced an `opaque` result: `duplicate1`, `ExtractSubset`, and both
`for()` loops.  It showed up as `sort()` on a `uint64` vector coming
back as hex.

This is the `ans_width` bug again one field later, and the fix is the
same shape: those sites now call `R_allocVectorLike()`, which carries
width *and* kind, and the width-only entry point is documented as the
public convenience form that internal code must not use.  Any future
per-vector property will have exactly this failure mode -- the
allocator is the choke point worth guarding.

## NA

For `opaque` and `unsigned`, NA is the all-`0xFF` element.  Reserving a pattern is what every other
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
address.  Hence the escape hatch below.

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

## The strict barrier

`--enable-strict-barrier` compiles R's own code *without*
`USE_RINTERNALS`, which is how it forces every field access through a
checked accessor.  All of the `BYTEVEC_*` macros were inside that
block in `Defn.h`, so the branch did not compile at all in that
configuration -- an entire supported build, not a corner of one.

They now sit outside it, split the way R splits `LENGTH()` and its
neighbours: macros when the fields are in reach, out-of-line functions
from `memory.c` when they are not.  Two consequences worth recording,
because both are the kind of thing the next new SEXPTYPE will hit:

  * the out-of-line accessors type-check first.  A width read out of
    some other SEXP's `gp` field is not a wild pointer, just a wrong
    small number, which is exactly the sort of bug that survives a long
    time.

  * `R_allocBytesVectorKind()` moved to `memory.c`.  It allocates a
    RAWSXP and retypes it, and the `SET_TYPEOF` the rest of R sees once
    the barrier is on whitelists a handful of conversions that do not
    include this one -- rightly, since that function is reachable from
    packages.  Allocation plumbing belongs next to the allocator
    anyway.

Both builds run the gauntlet clean.  Under the strict barrier the
`gctorture` sections are slow enough to be worth skipping when
iterating.
