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
    error(
      "Argument `hi` must be length 1 (is %s).", CSR_len_as_chr(xlength(hi))
    );
  if(xlength(lo) != 1)
    error(
      "Argument `lo` must be length 1 (is %s).", CSR_len_as_chr(xlength(lo))
    );
  if(xlength(include_ends) != 1)
    error(
      "Argument `include.ends` must be length 1 (is %s).",
      CSR_len_as_chr(xlength(include_ends))
    );

  if(scalar_na(hi)) error("Argument `hi` must not be NA.")
  if(scalar_na(lo)) error("Argument `lo` must not be NA.")

  if(TYPEOF(include_ends) != STRSXP || xlength(include_ends) != 1)
    error(
      "Argument `include.ends` must be character (is %s).",
      type2char(TYPEOF(include_ends))
    );
  if(STRING_ELT(include_ends, 0) == NA_STRING)
    error("Argument `include.ends` may not be NA.");

  const char * inc_end_chr = CHAR(STRING_ELT(include_ends, 0));
  int inc_lo = 0, inc_hi = 0;  // track whether to include bounds
  int bw = 0;  // track whether doing inside or outside

  if(CSR_strmlen(inc_end_chr, 3) != 2) include_end_err();

  if(inc_end_chr[0] == '[' || inc_end_chr[0] == '(') {
    bw = 1;
  } else if (inc_end_chr[0] == ']' || inc_end_chr[0] == ')') {
    bw = 0;
  } else {
    include_end_err();
  }
  if(inc_end_chr[1] == '[' || inc_end_chr[1] == '(') {
    if(bw) include_end_err();
  } else if (inc_end_chr[1] == ']' || inc_end_chr[1] == ')') {
    if(!bw) include_end_err()
  } else {
    include_end_err();
  }
  inc_lo = inc_end_chr[0] == '[' || inc_end_chr[0] == ']'
  inc_hi = inc_end_chr[1] == '[' || inc_end_chr[1] == ']'

  // - Numerics ----------------------------------------------------------------

  if(num_like(x))  {
    if(!num_like(lo))
      error(
        "Argument `x` is numeric-like, but `lo` is %s.", type2char(lo_type)
      );
    if(!num_like(hi))
      error(
        "Argument `x` is numeric-like, but `hi` is %s.", type2char(hi_type)
      );

    double lo_num = asReal(lo);
    double hi_num = asReal(hi);
    if(lo_num > hi_num) {
      hi_num = lo_num;
      lo_num = asReal(hi);
    }


    if(x_type == INTSXP) {
      int lo_int = asInteger(lo);
      int hi_int = asInteger(hi);

      if(lo_int = NA_INTEGER)
        error("Argument `lo` is NA when coerced to integer.");
      if(hi_int = NA_INTEGER)
        error("Argument `hi` is NA when coerced to integer.");

      for(R_xlen_t i = 0; i < xlength(x); ++i) {
        if(INTEGER(x)[i] > hi_int)
          return ScalarString(
            CSR_smprintf4(
              10000, "only contain values less than %s (%s found at index %s)",
              CSR_num_as_chr(hi, 1), CSR_num_as_chr(INTEGER(x)[i], 1),
              CSR_len_as_chr(i, 1)
            )
          );
      }
    } else if (x_type == REALSXP) {
    } else error("Internal Error: invalid `x` type %s.", type2char(x_type));

  } else if(x_type == STRSXP) {

  } else {
    error(
      "Argument `x` must be numeric-like or character (is %s).",
      type2char(x_type)
    );
  }
  return R_NilValue;
}
