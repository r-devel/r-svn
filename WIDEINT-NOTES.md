# Wide (64-bit) integer vector prototype

Dual-representation INTSXP experiment: a standard (non-ALTREP) INTSXP
whose payload holds `R_wideint_t` elements (a typedef, currently
`long long`), tagged with gp bit 7 (`WIDEINT_MASK`, free on vectors).
NA is `INT64_MIN`, mirroring `NA_INTEGER == INT32_MIN`. Code printing
a wide value with `"%lld"` casts to `long long` explicitly, so the
typedef can change without breaking formats.

## Design rules

- One integer type at the R level: `typeof()` stays "integer",
  `is.integer()` is TRUE. Wide-ness is a storage property, not a type.
- No 64-bit pointer accessor, by design. The 64-bit API is
  element-based only: `INTEGER64_ELT` / `SET_INTEGER64_ELT`. This
  sidesteps the write-back/aliasing problem entirely.
- The 32-bit pointer accessors (`INTEGER()`, `INTEGER_RO()`,
  `INTEGER0()`) error loudly on wide vectors -- both the package-facing
  functions and the core macros in Defn.h route through
  `R_INTEGER32chk*()`. There is no silent misread path.
- `INTEGER_ELT` on a wide vector narrows per element: succeeds for
  values representable in 32 bits, errors otherwise. So old ELT-based
  code keeps working until a big value actually reaches it.
- `INTEGER_OR_NULL` returns NULL on wide vectors (callers then use the
  region path); `INTEGER_GET_REGION` narrows element-wise with the
  same per-element rule.
- Narrow arithmetic overflow (`+`, `-`, `*`) promotes to a wide result
  instead of NA-with-warning. To keep the redo sound, those three ops
  no longer reuse an input vector as the result buffer.
- Wide arithmetic overflow (past 64 bits) produces NA64 + warning,
  mirroring narrow semantics pre-promotion.
- Mixed wide/double arithmetic coerces to double (precision warning
  above 2^53). Comparisons wide-vs-double are exact (no round trip
  through double).
- `coerceVector(wide, INTSXP)` is the identity: `as.integer()` of a
  wide vector stays wide. One type.
- Serialization of wide vectors errors (format work out of scope);
  unserialization clears the bit defensively.
- Wide vectors are allocated via `allocVector(REALSXP, n)` + relabel,
  so GC node classes and heap accounting see the true 8-byte payload;
  `getVecSizeInVEC` is wide-aware. Scalar flag is kept off to avoid
  32-bit scalar fast paths. The relabel trick assumes
  `sizeof(long long) == sizeof(double)` (a `_Static_assert` in
  memory.c enforces this); a real implementation should instead grow
  an internal allocator taking an explicit payload element size
  (an `allocVector0(size, n)`), which removes the coupling and keeps
  type-checking instrumentation honest.

## Test hooks

- `.Internal(as.wideint(x))` -- from logical/integer/double/character
  (strings allow exact big values like "9007199254740993").
- `.Internal(is.wideint(x))`.

## Files touched

- src/include/Defn.h -- bit, macros, guarded INTEGER()/INTEGER_RO(),
  declarations
- src/include/Rinternals.h, Rinlinedfuns.h -- API decls, wide-aware
  INTEGER_ELT/SET_INTEGER_ELT/INTEGER0/INTEGER_OR_NULL
- src/main/memory.c -- checked accessors, allocWideIntVector, ELT64,
  GC size accounting
- src/main/wideint.c -- .Internal hooks, coercion, formatting (new)
- src/main/duplicate.c, coerce.c, printvector.c, serialize.c,
  subset.c, relop.c, arithmetic.c -- wide paths
- src/main/names.c, src/include/Internal.h, src/main/Makefile.in

## Parser

The `L` suffix now produces a wide integer when the value does not
fit in 32 bits: `5000000000L`, `9007199254740993L` (parsed via
strtoll for exactness above 2^53), `0x1FFFFFFFFL`, `1e10L`.  Values
outside 64 bits, or with fractional parts, keep the old
warn-and-use-numeric behavior.  deparse() emits plain L literals, so
values round-trip exactly; storage width intentionally does not
round-trip (a small value reparses narrow), which is consistent with
identical() comparing values, not widths.

## Coverage status (after waves 1-4)

All ~155 gauntlet probes pass except two intentional errors
(fractional as.wideint(), serialization).  Covered beyond the initial
prototype: the display layer (format/cat/deparse/sprintf/str, named
vectors, matrices, data.frames), asInteger/asReal/asXLength,
summaries (mean/min/max/range/prod/cumsum/cummax/cummin/which.max),
abs, bitwNot/And/Or/Xor, subassignment everywhere (vector/[[/matrix/
array/stretch) with narrow-to-wide promotion, rep, matrix(), rbind/
cbind, sort/order/rank (comparison sorts; radix is excluded at the R
level since it works on 32-bit keys), unique/duplicated/match/%in%/
factor/table via a two-scheme integer hash (ihash32 for all-narrow
inputs, width-agnostic ihash64 whenever a wide vector participates;
the schemes disagree for negative values, so the choice is made once
per hash table, and lookups branch on the scheme the table was built
with), and the byte-code STEPFOR/VECSUBSET fast paths.

Notable systemic fixes found while working through the failures:
- DATAPTR_OR_NULL / INTEGER_OR_NULL return NULL for wide vectors, so
  ITERATE_BY_REGION users take the guarded region path (this closed
  the only silent-corruption hole: sum/min/max read wide payloads as
  int32 pairs).
- ALTREP wrappers (wrap_meta) refuse wide vectors: the alt bit would
  hide the wide bit and present the payload as 32-bit.
- The fastpass_sortcheck() quick-sequence scan and the byte-code
  scalar fast paths bail to the generic handlers for wide input.

Still unsupported (by design or out of scope): serialization, radix
sort on wide keys, %o%/%x% conversions beyond sprintf, split/tapply
and friends were not explicitly probed.

## Gauntlet

`wideint-gauntlet.R` probes ~155 base operations; run it after any
change and diff the ok/ERR pattern.
