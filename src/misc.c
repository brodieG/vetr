#include "validate.h"

// - Testing Function ----------------------------------------------------------

SEXP VALC_test(SEXP a, SEXP b) {
  // VALC_stop2(a, "error!", b);
  Rprintf("hello\n");
  Rprintf("a: %s, b: %s\n", type2char(TYPEOF(a)), type2char(TYPEOF(b)));
  SEXP found = PROTECT(findVar(a, b));
  SEXP new_call = LCONS(VALC_SYM_paren, found);
  PrintValue(new_call);
  Rprintf("found type %s:", type2char(TYPEOF(found)));
  UNPROTECT(1);
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
}/*
Creat simple error for a tag
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
