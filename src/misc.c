/*
Copyright (C) 2017  Brodie Gaslam

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

#include "validate.h"

/* testing funs
SEXP VALC_test1(SEXP a) {
  return ScalarReal(REAL(a)[0] + REAL(a)[0] + REAL(a)[0]);
}
SEXP VALC_test2(SEXP a, SEXP b) {
  return ScalarReal(REAL(a)[0] + REAL(b)[0] + REAL(a)[0]);
}
SEXP VALC_test3(SEXP a, SEXP b, SEXP c) {
  return ScalarReal(REAL(a)[0] + REAL(b)[0] + REAL(b)[0]);
}
*/

// - Helper Functions ----------------------------------------------------------

int IS_LANG(SEXP x) {
  return(
    TYPEOF(x) == LANGSXP || TYPEOF(x) == SYMSXP ||
    (isVectorAtomic(x) && XLENGTH(x) == 1) || x == R_NilValue
  );
}
/*
Fake `stop`

Main benefit is that it allows us to control the call that gets displayed.
*/
void VALC_stop(SEXP call, const char * msg) {
  SEXP quot_call = list2(VALC_SYM_quote, call);
  SET_TYPEOF(quot_call, LANGSXP);
  SEXP cond_call = PROTECT(
    list3(install("simpleError"), ScalarString(mkChar(msg)), quot_call)
  );
  SET_TYPEOF(cond_call, LANGSXP);
  SEXP cond = PROTECT(eval(cond_call, R_GlobalEnv));
  SEXP err_call = PROTECT(list2(install("stop"), cond));
  SET_TYPEOF(err_call, LANGSXP);
  UNPROTECT(3);
  eval(err_call, R_GlobalEnv);
  // nocov start
  error("Internal Error: 3423; contact maintainer.");
} // nocov end

/*
Create simple error for a tag
*/
void VALC_arg_error(SEXP tag, SEXP fun_call, const char * err_base) {
  if(TYPEOF(tag) != SYMSXP) {
    // nocov start
    error(
      "Internal Error: %s%s"
      "non symbol arg names are not currently supported; ",
      "contact maintainer."
    );
    // nocov end
  }

  const char * err_tag = CHAR(PRINTNAME(tag));
  char * err_msg = R_alloc(
    strlen(err_base) - 2 + strlen(err_tag) + 1, sizeof(char)
  );
  sprintf(err_msg, err_base, err_tag);
  VALC_stop(fun_call, err_msg);
  // nocov start
  error("Internal Error: shouldn't get here 181; contact maintainer.");// nocov
} // nocov end
/*
return
 *  3 if zero length, and hence true in the way all(logical()) is TRUE
 *  2 if isTRUE,
 *  1 if every element is TRUE,
 *  0 if there is at least one FALSE,
 * -1 if identical to FALSE,
 * -2 if not logical,
 * -3 if a single NA,
 * -4 if contains NAs,
 * -6 if the result is a string and has at least one element
*/

int VALC_all(SEXP vec) {
  if(TYPEOF(vec) == STRSXP && xlength(vec)) return -6;
  if(TYPEOF(vec) != LGLSXP) return -2;
  int * vec_c = LOGICAL(vec);
  R_xlen_t i, i_end = XLENGTH(vec);

  if(!i_end) return 3;
  for(i = 0; i < i_end; i++) {
    if(vec_c[i] == NA_INTEGER)
      return i_end == 1 ? -3 : -4;
    if(vec_c[i] != 1) return i_end == 1 ? -1 : 0;
  }
  if(i_end == 1) return 2;
  return 1;
}
/*
ext interface for testing
*/
SEXP VALC_all_ext(SEXP vec) {
  return ScalarInteger(VALC_all(vec));
}

/*
print current PROTECT stack height; used for debugging
*/
// nocov start
void psh(const char * lab) {
  PROTECT_INDEX i;
  PROTECT_WITH_INDEX(R_NilValue, &i);
  UNPROTECT(1);
  Rprintf("Protect Stack %s: %d\n", lab, i);
}
// nocov end
