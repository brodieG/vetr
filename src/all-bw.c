#include "all-bw.h"

static int num_like(SEXP x) {
  return TYPEOF(x) == REALSXP || TYPEOF(x) == INTSXP;
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
    "\"[]\", \"[)\", \"(]\", \"()\", \"][\", \"](\", \")[\", \")(\".";
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
  SEXP x, SEXP hi, SEXP lo, SEXP na_rm, SEXP include_bounds
) {
  SEXPTYPE x_type = TYPEOF(x), lo_type = TYPEOF(lo), hi_type = TYPEOF(hi);

  // - Validation --------------------------------------------------------------

  // Note we use char version of number to avoid portability issues with zd and
  // similar on MinGW

  if(xlength(na_rm) != 1)
    error(
      "Argument `na_rm` must be length 1 (is %s).",
      CSR_len_as_chr(xlength(na_rm))
    );
  if(TYPEOF(na_rm) != LGLSXP) {
    error(
      "Argument `na_rm` must be logical (is %s).",
      type2char(TYPEOF(na_rm))
    );
  }
  int na_rm_int = asInteger(na_rm);
  if(!(na_rm_int == 1 || na_rm_int == 0))
    error("Argument `na_rm` must be TRUE or FALSE (is NA).");

  if(xlength(hi) != 1)
    error(
      "Argument `hi` must be length 1 (is %s).", CSR_len_as_chr(xlength(hi))
    );
  if(xlength(lo) != 1)
    error(
      "Argument `lo` must be length 1 (is %s).", CSR_len_as_chr(xlength(lo))
    );
  if(xlength(include_bounds) != 1)
    error(
      "Argument `include.ends` must be length 1 (is %s).",
      CSR_len_as_chr(xlength(include_bounds))
    );

  if(scalar_na(hi)) error("Argument `hi` must not be NA.");
  if(scalar_na(lo)) error("Argument `lo` must not be NA.");

  if(TYPEOF(include_bounds) != STRSXP || xlength(include_bounds) != 1)
    error(
      "Argument `include.ends` must be character (is %s).",
      type2char(TYPEOF(include_bounds))
    );
  if(STRING_ELT(include_bounds, 0) == NA_STRING)
    error("Argument `include.ends` may not be NA.");

  const char * inc_end_chr = CHAR(STRING_ELT(include_bounds, 0));
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
    if(!bw) include_end_err();
  } else {
    include_end_err();
  }
  inc_lo = inc_end_chr[0] == '[' || inc_end_chr[0] == ']';
  inc_hi = inc_end_chr[1] == '[' || inc_end_chr[1] == ']';

  // - Numerics ----------------------------------------------------------------

  // We end up doing doubles and ints completely separately, even though they
  // share the same logic to avoid coercing integers to doubles

  R_xlen_t i;
  int success = 1;
  char * err_val;

  if(num_like(x))  {
    if(!num_like(lo))
      error(
        "Argument `x` is numeric-like, but `lo` is %s.", type2char(lo_type)
      );
    if(!num_like(hi))
      error(
        "Argument `x` is numeric-like, but `hi` is %s.", type2char(hi_type)
      );

    // Determine low and high bounds, when using double bounds for integer `x`
    // need to adjust whether ends are included or not

    double lo_num = asReal(lo);
    double hi_num = asReal(hi);

    if(lo_num > hi_num) {
      error("Argument `hi` must be greater than or equal to `lo`.");
    }
    if(lo_num == -INFINITY && !bw)
      error(
        "Argument `lo` cannot be -infinity when using %s",
        "`include.ends` in \"][\", \"](\", \")[\", \")(\"."
      );
    if(hi_num == INFINITY && !bw)
      error(
        "Argument `hi` cannot be infinity when using %s",
        "`include.ends` in \"][\", \"](\", \")[\", \")(\"."
      );

    int lo_unbound = bw && lo_num == -INFINITY;
    int hi_unbound = bw && hi_num == INFINITY;

    const char * log_err =
      "Internal Error: unexpected logical result %s, contact maintainer.";

    // We're using the negated comparisons (e.g. `!(x > i)` since that allows a
    // natural resolution of NAs and NaNs without having to explicitly check for
    // them; the flipside is that we've got one extra operation (negation) on
    // every element; actually, not sure this actually make sense; if we flip
    // the comparison should still get the same result with NAs

    if(x_type == REALSXP) {
      // - Numeric -------------------------------------------------------------

      // probably for not between we just switch lo and high
      warning("remember to adjust for between and not between");

      double * data = REAL(x);

      if(!lo_unbound && !hi_unbound) {
        if(!inc_lo && !inc_hi) {
          if(na_rm_int) {
            for(i = 0; i < xlength(x); ++i) {
              if(
                !(ISNAN(data[i]) || (data[i] > lo_num && data[i] < hi_num))
              ) {
                success = 0;
                break;
            } }
          } else {
            for(i = 0; i < xlength(x); ++i) {
              if(!(data[i] > lo_num && data[i] < hi_num)) {
                success = 0;
                break;
          } } }
        } else if (inc_lo && inc_hi) {
          if(na_rm_int) {
            for(i = 0; i < xlength(x); ++i) {
              if(
                !(ISNAN(data[i]) || (data[i] >= lo_num && data[i] <= hi_num))
              ) {
                success = 0;
                break;
            } }
          } else {
            for(i = 0; i < xlength(x); ++i) {
              if(!(data[i] >= lo_num && data[i] <= hi_num)) {
                success = 0;
                break;
          } } }
        } else if (inc_lo) {
          if(na_rm_int) {
            for(i = 0; i < xlength(x); ++i) {
              if(
                !(ISNAN(data[i]) || (data[i] >= lo_num && data[i] < hi_num))
              ) {
                success = 0;
                break;
            } }
          } else {
            for(i = 0; i < xlength(x); ++i) {
              if(!(data[i] >= lo_num && data[i] < hi_num)) {
                success = 0;
                break;
          } } }
        } else if (inc_hi) {
          if(na_rm_int) {
            for(i = 0; i < xlength(x); ++i) {
              if(
                !(ISNAN(data[i]) || (data[i] > lo_num && data[i] <= hi_num))
              ) {
                success = 0;
                break;
            } }
          } else {
            for(i = 0; i < xlength(x); ++i) {
              if(!(data[i] > lo_num && data[i] <= hi_num)) {
                success = 0;
                break;
          } } }
        } else error(log_err, "q34");
      } else if (lo_unbound && hi_unbound) {
        success = 1;
      } else if (lo_unbound) {
        if(!inc_hi) {
          if(na_rm_int) {
            for(i = 0; i < xlength(x); ++i) {
              if(!(ISNAN(data[i]) || data[i] < hi_num)) {
                success = 0; break;
            } }
          } else {
            for(i = 0; i < xlength(x); ++i) {
              if(!(data[i] < hi_num)) {
                success = 0; break;
          } } }
        } else if (inc_hi) {
          if(na_rm_int) {
            for(i = 0; i < xlength(x); ++i) {
              if(!(ISNAN(data[i]) || data[i] <= hi_num)) {
                success = 0; break;
            } }
          } else {
            for(i = 0; i < xlength(x); ++i) {
              if(!(data[i] <= hi_num)) {
                success = 0; break;
          } } }
        }  else error(log_err, "q243oij");
      } else if (hi_unbound) {
        if(!inc_lo) {
          if(na_rm_int) {
            for(i = 0; i < xlength(x); ++i) {
              if(!(ISNAN(data[i]) || (data[i] > lo_num))) {
                success = 0; break;
            } }
          } else {
            for(i = 0; i < xlength(x); ++i) {
              if(!(data[i] > lo_num)) {
                success = 0; break;
          } } }
        } else if (inc_lo) {
          if(na_rm_int) {
            for(i = 0; i < xlength(x); ++i) {
              if(!(ISNAN(data[i]) || data[i] >= lo_num)) {
                success = 0; break;
            } }
          } else {
            for(i = 0; i < xlength(x); ++i) {
              if(!(data[i] >= lo_num)) {
                success = 0; break;
          } } }
        }  else error(log_err, "2945asdf");
      } else error(log_err, "hfg89");

      if(!success) {
        char * msg = CSR_smprintf6(
          10000, "contain only values in range %c%s%s%c (%s at index %s)",
          inc_end_chr[0],
          CSR_num_as_chr(lo_num, 0),
          CSR_num_as_chr(hi_num, 0),
          inc_end_chr[1],
          CSR_num_as_chr((double) data[i], 0),
          CSR_len_as_chr(i)
        );
        return ScalarString(msg);
      }
    } else if(x_type == INTSXP) {
      // - Integer -------------------------------------------------------------
      int lo_int, hi_int;

      if(lo_num < INT_MIN) {
        lo_int = INT_MIN;
        lo_unbound = 1;
      } else {
        lo_int = asInteger(lo);
      }
      if(hi_num > INT_MAX) {
        hi_int = INT_MAX;
        hi_unbound = 1;
      } else {
        hi_int = asInteger(hi);
      }
      if(lo_int == NA_INTEGER || hi_int == NA_INTEGER)
        // nocov start
        error("Internal Error: int bounds ended up NA, contact maintianer.");
        // nocov end

      // When specifying double bounds for integer `x`, can affect whether to
      // use greater than or equal vs greater than (and same for less than)

      if(lo_num > (double)lo_int) {
        inc_lo = 0;
      }
      if(hi_num > (double)hi_int) {
        inc_hi = 1;
      }
      error("Integers not implemented yet");
    }
  } else if(x_type == STRSXP) {
    // - Strings ---------------------------------------------------------------

    error("Strings not implemented yet");
  } else {
    error(
      "Argument `x` must be numeric-like or character (is %s).",
      type2char(x_type)
    );
  }
  return ScalarLogical(1);
}
