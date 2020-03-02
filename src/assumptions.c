/*
Copyright (C) 2020 Brodie Gaslam

This file is part of "vetr - Trust, but Verify"

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
*/

#include <float.h>
#include <stdint.h>
#include <Rinternals.h>

/*
 * Check all the assumptions we're making
 *
 * Intended to be run onload to make sure there isn't some weird system where
 * our baseline assumptions are not met
 *
 * returns TRUE on success, errors on failure
 */
// nocov start by definition none of the errors should be thrown, so no sense in
// covering this
SEXP VALC_check_assumptions() {
  const char * err_base = "Failed system assumption: %s%s";
  if(sizeof(R_len_t) < sizeof(int))
    warningcall(R_NilValue, err_base, "R_len_t is not gte to int", "");

  // Otherwise bit twiddling assumptions may not work as expected?

  if(CHAR_BIT != 8)
    warningcall(R_NilValue, err_base, "CHAR_BIT is not 8", "");

  // This is supposedly enforced by R

  if(sizeof(int) < 4)
    warningcall(R_NilValue, err_base, "ints are not at least 32 bits", "");

  // If this is not TRUE, there could be alignment issues for some of our
  // structs that use size_t elements given that R_alloc only guarantees double
  // alignment.
  //
  // This will likely cause problems on systems other than 32 and 64 bits,
  // particularly those with larger register sizes, probably the easiest
  // solution is to not use size_t in the structs if this becomes a problem

  if(sizeof(size_t) > sizeof(double))
    warningcall(R_NilValue, err_base, "size_t larger than double not same size");

  // Important for some our boundary condition assumptions, in particular that
  // NA_INTEGER < int x.

  if(INT_MIN != NA_INTEGER) {
    warningcall(
      R_NilValue, err_base, "INT_MIN != NA_INTEGER but the code in this ",
      "package assumes that they are equal; please contact maintainer."
    );
  }
  // Mostly because we try to represent R_xlen_t values with %.0f

  if(R_XLEN_T_MAX >= DBL_MAX)
    warningcall(R_NilValue, err_base, "R_XLEN_T_MAX is not less than DBL_MAX");

  if(sizeof(R_len_t) != sizeof(int))
    warningcall(R_NilValue, err_base, "R_len_t not same size as int", "");

  // Because we check that strings are no longer than this, but then allocate
  // memory as INT_MAX + 1 with a size_t, so need to make sure that fits

  if(SIZE_MAX - 1 < INT_MAX)
    warningcall(
      R_NilValue, err_base,
      "SIZE_MAX not sufficiently larger than INT_MAX", ""
    );
  // Because sometimes we use size_t to hold positive R_LEN_T_MAX values

  if(SIZE_MAX <= R_LEN_T_MAX)
    warningcall(
      R_NilValue, err_base,
      "SIZE_MAX smaller than or equal to R_LEN_T_MAX", ""
    );
  return ScalarLogical(1);
}
// nocov end
