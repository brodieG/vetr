#include "validate.h"
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
  {"validate", (DL_FUNC) &VALC_validate, 7},
  {"validate_args", (DL_FUNC) &VALC_validate_args, 3},
  {"test", (DL_FUNC) &VALC_test, 2},
  {"name_sub", (DL_FUNC) &VALC_name_sub_ext, 2},
  {"symb_sub", (DL_FUNC) &VALC_sub_symbol, 2},
  {"parse", (DL_FUNC) &VALC_parse, 3},
  {"remove_parens", (DL_FUNC) &VALC_remove_parens, 1},
  {"eval_check", (DL_FUNC) &VALC_evaluate, 6},
  {"test1", (DL_FUNC) &VALC_test1, 1},
  {"test2", (DL_FUNC) &VALC_test2, 2},
  {"all", (DL_FUNC) &VALC_all_ext, 1},
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
  VALC_SYM_current = install("current");
  VALC_SYM_errmsg = install("err.msg");
  VALC_TRUE = ScalarLogical(1);

  VALC_match_call = (SEXP(*)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP)) R_GetCCallable("matchcall", "MC_match_call_internal");
  VALC_alike = (SEXP(*)(SEXP, SEXP, SEXP, SEXP)) R_GetCCallable(
    "alike", "ALIKEC_alike_ext2"
  );
  VALC_merge_msg = (SEXP(*)(SEXP)) R_GetCCallable("alike", "ALIKEC_merge_msg");
  VALC_deparse = (SEXP(*)(SEXP, int)) R_GetCCallable("alike", "ALIKEC_deparse");
  VALC_pad = (const char * (*)(SEXP, int, int)) R_GetCCallable("alike", "ALIKEC_pad");
  VALC_pad_or_quote = (const char * (*)(SEXP, int, int)) R_GetCCallable("alike", "ALIKEC_pad_or_quote");
  VALC_get_frame_data = (SEXP(*)(SEXP,SEXP,SEXP,int)) R_GetCCallable("matchcall", "MC_get_frame_data");

  VALC_get_fun = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("matchcall", "MC_get_fun");
  CSR_strmcpy = (char * (*)(const char *, size_t)) R_GetCCallable("cstringr", "CSR_strmcpy");
  CSR_smprintf4 = (char * (*)(size_t, const char *, const char *, const char *, const char *, const char *))
    R_GetCCallable("cstringr", "CSR_smprintf4");
  CSR_strmlen = (size_t (*)(const char *, size_t)) R_GetCCallable("cstringr", "CSR_strmlen");
  VALC_bullet = (const char * (*)(const char *, const char *, const char *, size_t )) R_GetCCallable("cstringr", "CSR_bullet");
  CSR_collapse = (const char * (*)(SEXP, const char *, size_t)) R_GetCCallable("cstringr", "CSR_collapse");

}

