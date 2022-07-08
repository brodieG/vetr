/*
Copyright (C) 2022 Brodie Gaslam

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

#include "cstringr.h"
/*
 * Functions for testing corner cases in cstringr functions.  Here because
 * size_t may be system dependent so we cannot test the results directly from R
 * where we have no mechanism for generating size_t values.
 *
 * These are all expected to produce errors
 */

SEXP CSR_test_strmcpy() {
  size_t maxlen = 0;
  maxlen--; // size_t of max value
  CSR_strmcpy("hello", maxlen);
  return R_NilValue;  // nocov
}

SEXP CSR_test_strappend() {
  size_t maxlen = 0;
  maxlen--; // size_t of max value
  CSR_strappend("hello", "hello", maxlen);
  return R_NilValue;  // nocov
}

SEXP CSR_test_add_szt() {
  size_t maxlen = 0;
  maxlen--; // size_t of max value
  CSR_add_szt(maxlen, maxlen);
  return R_NilValue;  // nocov
}
/*
 * Make sure all the variations on CSR_smprintf6 actually work
 */
SEXP CSR_test_smprintfx() {
  return mkString(
    CSR_smprintf5(
      10000, "%s\n%s\n%s\n%s\n",
      CSR_smprintf4(10000, "%s %s %s %s", "a", "b", "c", "d"),
      CSR_smprintf3(10000, "%s %s %s", "a", "b", "c"),
      CSR_smprintf2(10000, "%s %s", "a", "b"),
      CSR_smprintf1(10000, "%s", "a"),
      "the END"
    )
  );
}
// Make sure warning generated correctly

SEXP CSR_test_strappend2() {
  char * str_new = R_alloc(20, sizeof(char));
  CSR_strappend(str_new, "hellothere", 5);
  return R_NilValue;
}
