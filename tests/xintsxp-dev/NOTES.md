# XINTSXP: current design and invariants

`XINTSXP` is the internal representation for signed or unsigned
fixed-width integer vectors whose elements have a per-vector byte width.
This note records the current design;
the commit history contains the discarded prototypes and implementation
stages.

The user-facing reference is `?xinteger`. The package-facing C interface
is documented under “xinteger vectors” in *Writing R Extensions*.

## Representation

A vector has four defining properties:

| Property | Representation |
| --- | --- |
| element count | `XLENGTH(x)` |
| width, 1/2/4/8/16 bytes | `gp` bits 8–15 |
| kind: unsigned or signed | `gp` bits 0–1 |
| whether an `NA` sentinel is reserved | inverse flag in `gp` bit 2 |

`XLENGTH()` counts elements, not bytes. The payload therefore occupies
`XLENGTH(x) * XINT_WIDTH(x)` bytes. This is why the representation is
a distinct `SEXPTYPE` rather than a strided `RAWSXP`, whose length is a
byte count.

Width, kind and the sentinel policy together form the element type.
Generic allocation with `allocVector(XINTSXP, n)` is invalid because it
does not provide those properties. Code that needs another vector like
an existing one uses `R_allocVectorLike()` or `R_allocMatrixLike()`.

The width is one of five: 1, 2, 4, 8 or 16 bytes. Every operation the
type defines works at all of them, so there is no second tier of widths
that can be stored and sorted but not added. The `gp` field stays a
whole byte regardless, both because the serialized encoding should not
depend on which widths are currently allowed and so that the set can be
widened without a format change.

The payload begins where any other vector's does, so it has the
alignment R gives a `double`, and an element sits at a multiple of the
width from there. Widths up to 8 therefore land on their own natural
alignment. Width 16 does not: a small vector's node is carved from a
page whose data starts `sizeof(PAGE_HEADER)` in, leaving the payload 8
past a 16-byte boundary, while a large one comes straight from
`malloc()` and is 16-aligned by accident. Forcing 16 would mean
relaying out every node in R, and would buy nothing measurable --
`Rcomplex` is 16 bytes and R does not align it either.

So a width of 8 or less can be read by casting the payload, which is
what *Writing R Extensions* tells package code that has checked the
width and kind; width 16 cannot, having neither a portable C type nor
the alignment. The kernels here use `memcpy()` at every width instead,
since they are written once for all five.

The `gp` fields are serialized before allocation, so the reader knows
the element size when it allocates the payload. The “no NA” flag is
inverted: a clear bit means the default policy, with an `NA` sentinel.

## R-level identity

All variants share the structural type reported by `typeof()`:

| Kind | Example `storage.mode()` | `mode()` | implicit `class()` |
| --- | --- | --- | --- |
| unsigned | `"uint64"` | `"numeric"` | `"uint64"` |
| signed | `"int64"` | `"numeric"` | `"int64"` |

For every row, `typeof(x)` is `"xinteger"`, and both `is.xinteger(x)` and
`is.numeric(x)` are true. `xintegerKind()` distinguishes signed and
unsigned values.

The detailed storage-mode name contains the kind and width but not the
sentinel policy. Interfaces that must preserve the complete element
type accept a prototype vector, or use `.vectorlike()` and `.arraylike()`
when allocating from R. For example, `.vectorlike(x, n)` preserves
`xintegerHasNA(x)`, whereas `vector(storage.mode(x), n)` uses the default
sentinel policy. Plain `"xinteger"` is incomplete and is not accepted as
a storage mode.

Assigning an implicit class back is a no-op. In particular,
`class(x) <- class(x)` leaves a bare vector bare.

## Kinds and storage order

Signed and unsigned elements are integers of `8 * width` bits. They
are stored in native byte order so a native value can be copied into
the payload directly, but they are ordered by numeric value and rendered
as decimal. Serialization normalizes numeric payloads to most-
significant-byte first. Thus sorting and serialized values are portable
even though in-memory numeric storage is native-endian.

Numeric `NA` sentinels follow the interpretation:

| Kind | Reserved value when `na = TRUE` |
| --- | --- |
| unsigned | maximum unsigned value |
| signed | most negative signed value |

With `na = FALSE`, every bit pattern is a value. An operation that
would need to create a missing value then raises an error. This
includes out-of-range or missing subscripts, `length<-` growth, join
misses and arithmetic overflow. Vectors with different sentinel
policies are different element types and cannot be combined, compared
or matched without explicit conversion.

## Room the representation leaves

The kind occupies two `gp` bits and two of its four values are used,
so another kind costs nothing in the representation. The obvious
candidate is IEEE floating point, principally `float16` and `float32`,
which reach R from Arrow, Parquet and HDF5 columns that today have to
be widened to double on the way in. Width 8 would duplicate `REALSXP`
and is only worth having for uniformity; width 16 is `float128`, where
a platform has one. The width field would need nothing: all four of
those are already in the set.

**None of this is implemented and none of it is proposed here.** It is
recorded only to show what the design does and does not foreclose.

Most of the machinery would carry over untouched: per-vector width,
native-order storage and memcpy ingest, serialization (width and kind
already ride in `gp` through `PackFlags`), containers, subsetting,
matrices and the allocation discipline. Four things would be real work
rather than free:

- **Ordering.** Float bytes do not compare as values. The byte radix
  needs the usual IEEE key transform first -- flip the sign bit of a
  positive, invert every bit of a negative -- so that the existing
  unsigned pass order agrees with numeric order.
- **The sentinel.** R already reserves a `NaN` payload for `NA_real_`
  and the same trick generalizes per width, but `float16` has only ten
  significand bits to hide one in.
- **Arithmetic.** Widths 4 and 8 have a C type to dispatch to; 2 and 16
  do not on most platforms, which is the shape the integer kinds
  already hit at width 16 without `__int128`.
- **Width 2 is ambiguous.** IEEE half and bfloat16 are both two bytes
  with different exponent splits. That is exactly the ambiguity the
  kind field exists to resolve, so it would consume both remaining
  kind codes.

Exact decimal is the other type R lacks, and it does *not* fit here: a
per-vector scale needs about six bits and only bits 3, 6 and 7 are
free. It would have to carry the scale in an attribute, which is the
`class(x) <- c("decimal", class(x))` route rather than a kind.

## Coercion and operations

Two `XINTSXP` operands must have the same width, kind and sentinel
policy unless an explicit conversion is requested. Widths are not
implicitly promoted.

Logical and integer operands narrow into a numeric fixed-width operand.
For arithmetic, an unrepresentable value becomes `NA` with a warning;
for comparison and matching, it is ordered outside the representable
range or treated as absent. Double and complex operands promote the
fixed-width value to the ordinary R type. Conversion to double warns
only when an actual value loses precision.

Arithmetic is defined at every width, as is everything else.
The integer kinds support subsetting, concatenation, comparison,
matching, hashing, sorting, tables, factors, arithmetic, summaries,
numeric coercion, `Math`, exact unit-step sequences and exact
accumulation for `mean()` before its final conversion to double.

Character conversion is reversible and decimal, which is why a
character operand promotes rather than being refused: `x == "1"`,
`match()` and `%in%` settle on character exactly as `c()` and
`x[i] <- value` do, so the same pair of operands cannot be accepted by
one and refused by another. `xintegerRaw()` exposes
storage order and is
therefore intended for round trips and native interfaces, not as a
portable numeric encoding.

## Allocation and C API

Packages allocate with:

```c
SEXP R_allocXIntVector(R_xlen_t n, int width, int kind,
                        Rboolean hasNA);
```

The public kinds are `XINT_UNSIGNED` and `XINT_SIGNED`.
`R_xintWidth()`, `R_xintKind()` and
`R_xintHasNA()` inspect the element type. `R_xintElt()` and
`R_xintEltRO()` address one element; `R_xintIsNA()` and
`R_xintSetNA()` handle missingness. `XINTEGER()` and `XINTEGER_RO()` expose
the whole payload.

Ordinary typed accessors (`INTEGER`, `REAL`, `RAW` and their element
forms) reject `XINTSXP` by type. The untyped `DATAPTR` family also
rejects it so code cannot read a payload without first accounting for
its width and kind. `XINTSXP` is not ALTREP.

## Serialization

Streams containing `XINTSXP` require serialization version 4. Writers
select version 4 automatically when no version was specified; an
explicit older version is rejected before a destination file is opened.
The save magic remains `RDX3` (or its ASCII/XDR sibling) because the
serialization header that follows carries the actual version.

Both decisions are made by walking the object before the header goes
out, because the header precedes the first item and a connection cannot
be rewound. The walk mirrors what the write would reach. An ALTREP
object is class, two data fields and attributes, so for one of R's own
classes the data fields are walked and the class is asked nothing; a
class from a package contributes whatever its `Serialized_state` method
builds, which cannot be settled without calling that method a second
time, so such an object selects version 4 unexamined. Erring that way
costs a version 4 file where a version 3 one would have done; erring
the other way discovers the type mid-stream with the destination
already truncated.

Payloads are normalized to big-endian element order on the wire.
Processing is chunked in whole elements so an element never straddles
a serialization chunk.

## Implementation invariants

- Never allocate a result with `allocVector(TYPEOF(x), n)` when `x`
  may be `XINTSXP`; use the “like” allocators so width, kind and sentinel
  policy survive.
- Never infer compatibility from `TYPEOF()` alone. Use
  `R_xintCheckSameType()` or the corresponding settlement helper.
- Check `XINT_HAS_NA(x)` before interpreting a sentinel pattern.
  Under `na = FALSE`, the same bits are an ordinary value.
- Payloads use native byte order.
- Preserve attributes through the same outer machinery used by the
  ordinary atomic types. Low-level kernels return bare vectors.
- A `XINTSXP` result must retain width, kind and sentinel policy through
  subsetting, iteration, binding, matrix operations and serialization.

## Test suites

`make test-XIntsXP` runs:

- `gauntlet.R`: public behavior and regression cases;
- `endcheck.R`: storage and wire byte order;
- `pcheck.R` and `xcheck.R`: value, text and ordering cross-checks;
- `archeck.R`: exact arithmetic and native/general kernel agreement;
- `realcheck.R`: correctly rounded conversion to double;
- `rxcheck.R`: radix ordering and stability with heavy ties.

The reference arithmetic in `bignum.R` uses decimal digit vectors and
shares no implementation with the binary-byte kernels. Each suite
runs its self-test before using it as an oracle. `make test-XIntsFFI`
separately checks the package-facing C boundary and guarded accessors.
