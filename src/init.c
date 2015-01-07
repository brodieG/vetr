#include "validate.h"
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
  {"validate", (DL_FUNC) &VALC_validate, 3},
  {"test", (DL_FUNC) &VALC_test, 2},
  {"name_sub", (DL_FUNC) &VALC_name_sub_ext, 2},
  {"parse", (DL_FUNC) &VALC_parse, 3},
  {"remove_parens", (DL_FUNC) &VALC_remove_parens, 1},
  {"eval_check", (DL_FUNC) &VALC_evaluate, 5},
  {"test1", (DL_FUNC) &VALC_test1, 1},
  {"test2", (DL_FUNC) &VALC_test2, 2},
  {NULL, NULL, 0}
};

void R_init_validate(DllInfo *info)
{
 /* Register the .C and .Call routines.
    No .Fortran() or .External() routines,
    so pass those arrays as NULL.
  */
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  VALC_SYM_quote = install("quote");
  VALC_SYM_deparse = install("deparse");
  VALC_SYM_one_dot = install(".");
  VALC_SYM_paren = install("(");
  VALC_TRUE = ScalarLogical(1);
  VALC_match_call = (SEXP(*)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP)) R_GetCCallable("matchcall", "MC_match_call_internal");
  VALC_alike = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("alike", "ALIKEC_alike_fast");
  VALC_get_frame_data = (SEXP(*)(SEXP,SEXP,SEXP,int)) R_GetCCallable("matchcall", "MC_get_frame_data");
  VALC_get_fun = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("matchcall", "MC_get_fun");
}

