# BYTESXP: vectors of fixed-width opaque data

Prototype of a new SEXPTYPE for values that existing vector types
cannot hold: hashes, UUIDs, IPv6 addresses, 128-bit database keys.
The type is a *storage* type, not a numeric type.  Elements are
compared and hashed as byte blocks and are never interpreted as
numbers, so there is no coercion hierarchy to join and no arithmetic
to define.

Status: **stage 3 plus numeric kinds and arithmetic**.  Everything a filtering workload needs works:
allocation, GC, printing, identity, NA, subsetting, subassignment,
`c()`, `rep()`, comparison, `match`/`unique`/`duplicated`/`split`,
`sort`/`order`/`rank`/`xtfrm`, `as.character`/`format`, and
`table`/`factor`, serialization, `deparse`, matrices
(`cbind`/`rbind`/`matrix`/`t`/`aperm`/`apply` and matrix printing);
and for the numeric kinds, `+ - * %/% %% /` `^`, unary minus,
`sum`/`prod`/`min`/`max`/`range`, and `as.integer`/`as.numeric`.
No `.Call` boundary is needed -- see below -- and the original stage
list is complete.

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

One real defect surfaced here: `nvec[]` in `memory.c`, the table that
says which SEXPTYPEs are vectors for the package-facing `LENGTH()`,
had BYTESXP marked as a non-vector, so `XLENGTH(x)` from package code
errored.  R's own code uses the unchecked macro and so never noticed.
Fixed; it is exactly the kind of entry a new SEXPTYPE has to remember.

## deparse and matrices

There is no literal syntax for a `bytes` vector, so `deparse` emits
the call that rebuilds it -- `as.bytes(as.raw(c(0x01, ...)), 8L,
"unsigned")` -- exactly as raw vectors deparse to `as.raw(c(...))`.
Empty vectors deparse to `bytes(0L, w, kind)`.  The payload is written
in storage order, so this round-trips on the machine that produced it;
the wire normalization is what makes *files* portable.

Known wart: a vector containing NA re-parses to the right value but
warns, because `as.bytes` legitimately flags the reserved pattern
arriving as data and cannot tell an intentional NA from a collision.
The fix is a suppression argument or a literal syntax; neither is worth
designing yet.

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
