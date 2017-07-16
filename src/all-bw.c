#include "validate.h"

static int num_like(SEXP x) {
  return TYPEOF(x) == NUMSXP || TYPEOF(x) == INTSXP;
}
/*
 * For the types we can discern NA_ness, check it for the first element.  Tends
 * to default to 0 even when there are NAs, so it's not intended as a final
 * check.
 */
static int scalar_na(SEXP x) {
  SEXPTYPE x_type = TYPEOF(x);
  int res = 1;

  if(xlength(x) == 1) {
    if(x_type == STRSXP) res = (STRING_ELT(x, 0) == NA_STRING);
    else if(x_type == INTSXP) res = INTEGER(x)[0] == NA_INTEGER;
    else if(x_type == REALSXP) res = REAL(x)[0] == NA_REAL;
  }
  return res;
}
/*
 * helper fun to throw error
 */
static void include_end_err() {
  const char * valid_ends =
    "\"[]\", \"[)\", \"(]\", \"()\", "\"][\", \"](\", \")[\", \")(\".";
  error(
   "%s%s",
   "Argument `include.ends` must be a two character string in ",
   valid_ends
  );
}
/*
 * See R interface fun for docs
 */
SEXP VALC_all_bw(
  SEXP x, SEXP hi, SEXP lo, SEXP na_rm, SEXP include_ends
) {
  SEXPTYPE x_type = TYPEOF(x), lo_type = TYPEOF(lo), hi_type = TYPEOF(hi);

  // - Validation --------------------------------------------------------------

  if(xlength(hi) != 1)
    error("Argument `hi` must be length 1 (is %.0f).", (double) xlength(hi))
  if(xlength(lo) != 1)
    error("Argument `lo` must be length 1 (is %.0f).", (double) xlength(lo))

  if(num_like(x))  {

  } else if(x_type == STRSXP) {

  } else {
    error(
      "Argument `x` must be numeric-like or character (is %s).",
      type2char(x_type)
    );
  }
  if(!num_like(lo))
    error("Argument `x` is numeric-like, `lo` is %s.", type2char(lo_type));
  if(!num_like(hi))
    error("Argument `x` is numeric-like, `hi` is %s.", type2char(hi_type));
  return R_NilValue;
}
