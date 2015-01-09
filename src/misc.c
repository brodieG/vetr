#include "validate.h"

// - Testing Function ----------------------------------------------------------

SEXP VALC_test(SEXP a, SEXP b) {
  SEXP found2 = PROTECT(findVar(install("yy"), b));
  Rprintf("getting once");
  PrintValue(eval(a, b));
  Rprintf("getting twice");
  PrintValue(eval(a, b));
  SEXP found = PROTECT(findVar(a, b));
  PrintValue(found);
  PrintValue(found2);
  Rprintf("found type %s seen %d\n", type2char(TYPEOF(found)), PRSEEN(found));
  Rprintf("found type %s seen %d\n", type2char(TYPEOF(found2)), PRSEEN(found2));
  // eval(a, b);
  // Rprintf("found type %s seen %d\n", type2char(TYPEOF(found)), PRSEEN(found));
  // SEXP c = PROTECT(LCONS(install("force"), list1(a)));
  // PrintValue(c);
  // eval(c, b);
  // Rprintf("found type %s seen %d\n", type2char(TYPEOF(found)), PRSEEN(found));
  // SET_PRSEEN(found, 1);
  // found = PROTECT(findVar(a, b));
  // PrintValue(found);

  // Rprintf("found type %s seen %d\n", type2char(TYPEOF(found)), PRSEEN(found));
  // PrintValue(found);

  UNPROTECT(2);
  return(R_NilValue);
}

SEXP VALC_test1(SEXP a) {
  error("stop!;;;;;");
  return(a);
}
SEXP VALC_test2(SEXP a, SEXP b) {
  VALC_stop2(a, "error!", b);
  return(a);
}
SEXP VALC_test3(SEXP a, SEXP b) {
  VALC_stop2(a, "error!", b);
  return(a);
}
// - Helper Functions ----------------------------------------------------------

int IS_TRUE(SEXP x) {
  return(TYPEOF(x) == LGLSXP && XLENGTH(x) == 1 && asLogical(x));
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
  error("Logic Error: should never get here; contact maintainer.");
}
void VALC_stop2(SEXP call, const char * msg, SEXP rho) {
  SEXP quot_call = list2(VALC_SYM_quote, call);
  SET_TYPEOF(quot_call, LANGSXP);
  SEXP cond_call = PROTECT(
    list3(install("simpleError"), ScalarString(mkChar(msg)), quot_call)
  );
  SET_TYPEOF(cond_call, LANGSXP);
  SEXP cond = PROTECT(eval(cond_call, rho));
  SEXP err_call = PROTECT(list2(install("stop"), cond));
  SET_TYPEOF(err_call, LANGSXP);
  UNPROTECT(3);
  eval(err_call, R_GlobalEnv);
  error("Logic Error: should never get here; contact maintainer.");
}
/*
Create simple error for a tag
*/
void VALC_arg_error(SEXP tag, SEXP fun_call, const char * err_base) {
  const char * err_tag = CHAR(PRINTNAME(tag));
  char * err_msg = R_alloc(
    strlen(err_base) - 2 + strlen(err_tag) + 1, sizeof(char)
  );
  sprintf(err_msg, err_base, err_tag);
  VALC_stop(fun_call, err_msg);
  error("Logic Error: shouldn't get here 181; contact maintainer.");
}
/*
return 1 if every element is TRUE, 0 if there is at least one FALSE, -1 if
identical to FALSE, -2 if not logical
*/

int VALC_all(SEXP vec) {
  if(TYPEOF(vec) != LGLSXP) return -2;
  int * vec_c = LOGICAL(vec);
  R_xlen_t i, i_end = XLENGTH(vec);

  for(i = 0; i < i_end; i++) {
    if(vec_c[i] != 1) return i_end == 1 ? -1 : 0;
  }
  return 1;
}
/*
ext interface for testing
*/
SEXP VALC_all_ext(SEXP vec) {
  return ScalarInteger(VALC_all(vec));
}
