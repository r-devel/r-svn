/* R no longer uses trio: the printf family comes from the UCRT, see
   src/gnuwin32/fixed/h/trioremap.h.  This file no longer replaces the C
   run-time's printf functions, so nothing pulls trio into R.dll; it stays
   until the build files stop building src/extra/trio. */

typedef int R_trio_compat_unused; /* ISO C forbids an empty translation unit */
