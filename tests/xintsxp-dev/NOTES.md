# ALTSXP int64/uint64 prototype

This branch recasts the fixed-width integer proposal as two built-in ALTREP
classes with a new opaque base `SEXPTYPE`, `ALTSXP`.

## Representation

- `TYPEOF(x)` is `ALTSXP`; `typeof(x)` reports `"alt"`.
- Every ALTSXP object is a genuine ALTREP object. ALTSXP has no standard
  vector payload and `allocVector(ALTSXP, n)` is invalid.
- The base `int64` and `uint64` classes use a native-endian `RAWSXP` as
  ALTREP `data1`. Their `Length` method converts its byte count to an element
  count.
- `storage.mode()` and the implicit class report `"int64"` or `"uint64"`.
  `R_isXInt()` recognizes only these two built-in classes, not arbitrary
  package-defined ALTSXP classes.
- The imported integer kernels retain their width, kind and optional-NA
  metadata. This prototype accepts only width 8.

The base classes expose exact elements through the ALTSXP `Elt`, `Set_elt`
and `Get_region` methods. Generic `DATAPTR()` on the outer object is refused.
The opt-in fixed-integer C API reaches the class's private raw backing store.

## Semantic method API

`R_ext/Altrep.h` adds `R_make_alt_class()` and setters for:

- `Element_size`, `Elt`, `Set_elt`, `Get_region`;
- `Binary_op`, `Unary_op`, `Compare`;
- `Hash`, `Format`, `Summary`, `Combine`.

Coercion uses the existing ALTREP `Coerce` method. A binary or comparison
method receives both operands plus an explicit dispatch receiver. The left
class gets the first opportunity. A `NULL` result declines; a distinct right
class then gets one opportunity, with the original operand order preserved.
The same class is never called twice. The FFI test creates two package
ALTSXP classes to pin these rules, including first refusal over ordinary real
promotion.

Base R consults the semantic hooks in arithmetic, comparison, coercion,
formatting and `format.info`, summaries, hashing and concatenation. The two
built-in classes use the fixed-integer implementation imported from the
proposal. Where a class declines, existing promotion or the built-in
fixed-integer fallback is used as appropriate.

## Serialization

The two built-in classes serialize through ordinary ALTREP class state. The
state contains portable big-endian raw bytes plus the integer kind and NA
policy. This round-trips in the existing serialization stream version 3;
there is no standard ALTSXP vector payload and no new stream version is
needed.

## Compatibility boundary

Existing package code sees a new unknown `SEXPTYPE`. An exhaustive
`switch(TYPEOF(x))` takes its existing default branch, `REAL()` and
`INTEGER()` reject it, and `DATAPTR()` is unavailable. This is deliberate:
code must either remain safely unaware or opt into ALTSXP/fixed-integer
interfaces.

The current branch is a feasibility prototype rather than a complete generic
opaque-vector design. A number of base internals imported from the integer
proposal still have built-in int64/uint64 fallbacks after the semantic hook.
A production ALTSXP API would need to audit those remaining structural switch
sites and define generic equality/subsetting/allocation contracts for package
classes.

## Tests

`make test-XIntsXP` runs:

- `gauntlet.R`: public behavior for both built-in classes;
- `endcheck.R`: value and portable wire byte order;
- `pcheck.R` and `xcheck.R`: independent decimal/payload/order oracles;
- `archeck.R`: exact arithmetic and native/general-kernel agreement;
- `realcheck.R`: correctly rounded conversion to double;
- `rxcheck.R`: stable radix ordering under heavy ties.

The independent reference arithmetic in `bignum.R` uses decimal digit
vectors and shares no code with the binary kernels. `make test-XIntsFFI`
separately checks unaware-package failure modes, the opt-in C API, ALTREP
serialization, and two-sided semantic dispatch.
