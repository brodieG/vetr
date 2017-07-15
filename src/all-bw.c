#include "validate.h"

static int num_like(SEXP x) {
  return TYPEOF(x) == NUMSXP || TYPEOF(x) == INTSXP;
}
SEXP VALC_all_bw(SEXP x, SEXP hi, SEXP lo, SEXP na_rm) {
  SEXPTYPE x_type = TYPEOF(x), lo_type = TYPEOF(lo), hi_type = TYPEOF(hi);

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
