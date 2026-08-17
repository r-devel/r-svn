# Wide (64-bit) integer vector prototype

Dual-representation INTSXP experiment: a standard (non-ALTREP) INTSXP
whose payload holds `long long` elements, tagged with gp bit 7
(`WIDEINT_MASK`, free on vectors). NA is `INT64_MIN`, mirroring
`NA_INTEGER == INT32_MIN`.

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
  32-bit scalar fast paths.

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

## Gauntlet

`wideint-gauntlet.R` probes ~100 base operations; the ok/ERR pattern
is the catalog of what a real implementation would still need to
touch.
