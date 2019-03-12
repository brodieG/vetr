#include "all-bw.h"

static int num_like(SEXP x) {
  return TYPEOF(x) == REALSXP || TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP;
}
/*
 * For the types we can discern NA_ness, check it for the first element.  Tends
 * to default to 1 even when there are NAs, so it's not intended as a final
 * check.
 */
static int scalar_na(SEXP x) {
  SEXPTYPE x_type = TYPEOF(x);
  int res = 1;

  if(xlength(x) == 1) {
    if(x_type == STRSXP) res = (STRING_ELT(x, 0) == NA_STRING);
    else if(x_type == INTSXP) res = (INTEGER(x)[0] == NA_INTEGER);
    else if(x_type == REALSXP) res = ISNAN(REAL(x)[0]);
  }
  return res;
}
/*
 * helper fun to throw error
 */
static void include_end_err() {
  const char * valid_ends =
    "\"[]\", \"[)\", \"(]\", \"()\"";
  error(
   "%s%s",
   "Argument `bounds` must be character(1L) in ", valid_ends
  );
} // nocov can't hit this line due to error
/*
 * See R interface fun for docs
 */
SEXP VALC_all_bw(
  SEXP x, SEXP lo, SEXP hi, SEXP na_rm, SEXP include_bounds
) {
  SEXPTYPE x_type = TYPEOF(x), lo_type = TYPEOF(lo), hi_type = TYPEOF(hi);

  int int_min = INT_MIN + 1;
  int int_max = INT_MAX;

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
      "Argument `bounds` must be length 1 (is %s).",
      CSR_len_as_chr(xlength(include_bounds))
    );

  if(
    !(
      lo_type == LGLSXP || lo_type == STRSXP || lo_type == REALSXP ||
      lo_type == INTSXP
    )
  )
    error(
      "Argument `lo` must be logical, integer, numeric, or character (is %s).",
      type2char(lo_type)
    );

  if(
    !(
      hi_type == LGLSXP || hi_type == STRSXP || hi_type == REALSXP ||
      hi_type == INTSXP
    )
  )
    error(
      "Argument `hi` must be logical, integer, numeric, or character (is %s).",
      type2char(hi_type)
    );

  if(scalar_na(hi)) error("Argument `hi` must not be NA.");
  if(scalar_na(lo)) error("Argument `lo` must not be NA.");

  if(TYPEOF(include_bounds) != STRSXP || xlength(include_bounds) != 1)
    error(
      "Argument `bounds` must be character (is %s).",
      type2char(TYPEOF(include_bounds))
    );
  if(STRING_ELT(include_bounds, 0) == NA_STRING)
    error("Argument `bounds` may not be NA.");

  const char * inc_end_chr = CHAR(STRING_ELT(include_bounds, 0));
  int inc_lo = 0, inc_hi = 0;  // track whether to include bounds

  if(CSR_strmlen(inc_end_chr, 3) != 2) include_end_err();

  if(
    !(inc_end_chr[0] == '[' || inc_end_chr[0] == '(') ||
    !(inc_end_chr[1] == ']' || inc_end_chr[1] == ')')
  ) {
    include_end_err();
  }
  inc_lo = inc_end_chr[0] == '[';
  inc_hi = inc_end_chr[1] == ']';

  // Need actualy strings to use with CSR_smprintf

  char * inc_lo_str = R_alloc(2, sizeof(char));
  inc_lo_str[0] = inc_end_chr[0];
  inc_lo_str[1] = '\0';
  char * inc_hi_str = R_alloc(2, sizeof(char));
  inc_hi_str[0] = inc_end_chr[1];
  inc_hi_str[1] = '\0';

  // - Numerics ----------------------------------------------------------------

  // We end up doing doubles and ints completely separately, even though they
  // share the same logic to avoid coercing integers to doubles

  R_xlen_t i;
  R_xlen_t x_len = xlength(x);
  int success = 1;
  const char * log_err =
    "Internal Error: unexpected logical result %s, contact maintainer.";

  const char * lo_as_chr = "";
  const char * hi_as_chr = "";

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

    lo_as_chr = CSR_num_as_chr(lo_num, 0);
    hi_as_chr = CSR_num_as_chr(hi_num, 0);

    if(lo_num > hi_num) {
      error(
        "Argument `hi` (%s) must be greater than or equal to `lo` (%s).",
        CSR_num_as_chr(hi_num, 0), CSR_num_as_chr(lo_num, 0)
      );
    }
    // See if either side is unbounded

    int lo_unbound = (lo_num == R_NegInf && inc_lo);
    int hi_unbound = (hi_num == R_PosInf && inc_hi);

    // We're using the negated comparisons (e.g. `!(x > i)` since that allows a
    // natural resolution of NAs and NaNs without having to explicitly check for
    // them; the flipside is that we've got one extra operation (negation) on
    // every element; actually, not sure this actually make sense; if we flip
    // the comparison should still get the same result with NAs (NOTE: looks
    // like compiler is smart enough to figure this out since getting rid of
    // negation doesn't change computation time)

    // Note we're explicitly unswitching loops.  Compiler doesn't seem to do as
    // good a job at as if we do it manually.  PITA.  There might be a compiler
    // setting that achieves the desired outcome, but then we start having
    // portability issues.  See DEVNOTES.
    //
    // https://stackoverflow.com/questions/1462710/can-c-compilers-optimize-if-statements-inside-for-loops
    // https://en.wikipedia.org/wiki/Loop_unswitching

    if(x_type == REALSXP) {
      // - Numeric -------------------------------------------------------------

      double * data = REAL(x);

      if(!lo_unbound && !hi_unbound) {
        if(!inc_lo && !inc_hi) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(
                !(ISNAN(data[i]) || (data[i] > lo_num && data[i] < hi_num))
              ) {
                success = 0;
                break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] > lo_num && data[i] < hi_num)) {
                success = 0;
                break;
          } } }
        } else if (inc_lo && inc_hi) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(
                !(ISNAN(data[i]) || (data[i] >= lo_num && data[i] <= hi_num))
              ) {
                success = 0;
                break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] >= lo_num && data[i] <= hi_num)) {
                success = 0;
                break;
          } } }
        } else if (inc_lo) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(
                !(ISNAN(data[i]) || (data[i] >= lo_num && data[i] < hi_num))
              ) {
                success = 0;
                break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] >= lo_num && data[i] < hi_num)) {
                success = 0;
                break;
          } } }
        } else if (inc_hi) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(
                !(ISNAN(data[i]) || (data[i] > lo_num && data[i] <= hi_num))
              ) {
                success = 0;
                break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] > lo_num && data[i] <= hi_num)) {
                success = 0;
                break;
          } } }
        } else error(log_err, "q34");  // nocov
      } else if (lo_unbound && hi_unbound) {
        if(na_rm_int) success = 1;
        else {
          for(i = 0; i < x_len; ++i) {
            if(ISNAN(data[i])) {
              success = 0;
              break;
        } } }
      } else if (lo_unbound) {
        if(!inc_hi) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(!(ISNAN(data[i]) || data[i] < hi_num)) {
                success = 0; break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] < hi_num)) {
                success = 0; break;
          } } }
        } else if (inc_hi) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(!(ISNAN(data[i]) || data[i] <= hi_num)) {
                success = 0; break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] <= hi_num)) {
                success = 0; break;
          } } }
        }  else error(log_err, "q243oij");  // nocov
      } else if (hi_unbound) {
        if(!inc_lo) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(!(ISNAN(data[i]) || (data[i] > lo_num))) {
                success = 0; break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] > lo_num)) {
                success = 0; break;
          } } }
        } else if (inc_lo) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(!(ISNAN(data[i]) || data[i] >= lo_num)) {
                success = 0; break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] >= lo_num)) {
                success = 0; break;
          } } }
        }  else error(log_err, "2945asdf");  // nocov
      } else error(log_err, "hfg89");  // nocov

    } else if(x_type == INTSXP || x_type == LGLSXP) {
      // - Integer -------------------------------------------------------------
      int lo_int, hi_int;

      if(lo_num < int_min) {
        lo_int = int_min;
        lo_unbound = 1;
      } else {
        lo_int = asInteger(lo);
      }
      if(hi_num > int_max) {
        hi_int = int_max;
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

      if(lo_num < (double)lo_int) {
        inc_lo = 1;
      }
      if(hi_num > (double)hi_int) {
        inc_hi = 1;
      }
      // Re-use same symbols from double so we can use the exact same code
      // One annoying difference with INT is that NA_INTEGER is actually
      // INT_MIN, so we have to explicitly check the difference.  Since we're
      // re-using same logic as nums this is now a little more convoluted than
      // expected.  Additionally, some questions now whether we would be better
      // off just doing a coercion to double and using double logic (probably
      // not b/c still have to deal with the NA_INTEGER business).
      //
      // Re: above, looks like we only need to worry about it in the lo_unbound
      // case.

      int * data = INTEGER(x);
      int lo_num = lo_int;
      int hi_num = hi_int;

      if(!lo_unbound && !hi_unbound) {
        if(!inc_lo && !inc_hi) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(
                !(
                  (data[i] == NA_INTEGER) ||
                  (data[i] > lo_num && data[i] < hi_num))
              ) {
                success = 0;
                break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] > lo_num && data[i] < hi_num)) {
                success = 0;
                break;
          } } }
        } else if (inc_lo && inc_hi) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(
                !(
                  (data[i] == NA_INTEGER) ||
                  (data[i] >= lo_num && data[i] <= hi_num)
                )
              ) {
                success = 0;
                break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] >= lo_num && data[i] <= hi_num)) {
                success = 0;
                break;
          } } }
        } else if (inc_lo) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(
                !(
                  (data[i] == NA_INTEGER) ||
                  (data[i] >= lo_num && data[i] < hi_num))
              ) {
                success = 0;
                break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] >= lo_num && data[i] < hi_num)) {
                success = 0;
                break;
          } } }
        } else if (inc_hi) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(
                !(
                  (data[i] == NA_INTEGER) ||
                  (data[i] > lo_num && data[i] <= hi_num)
                )
              ) {
                success = 0;
                break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] > lo_num && data[i] <= hi_num)) {
                success = 0;
                break;
          } } }
        } else error(log_err, "intq34");  // nocov
      } else if (lo_unbound && hi_unbound) {
        if(na_rm_int) success = 1;
        else {
          for(i = 0; i < x_len; ++i) {
            if(data[i] == NA_INTEGER) {
              success = 0;
              break;
        } } }
      } else if (lo_unbound) {
        if(!inc_hi) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(!((data[i] == NA_INTEGER) || data[i] < hi_num)) {
                success = 0; break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] < hi_num && data[i] != NA_INTEGER)) {
                success = 0; break;
          } } }
        } else if (inc_hi) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(!((data[i] == NA_INTEGER) || data[i] <= hi_num)) {
                success = 0; break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] <= hi_num && data[i] != NA_INTEGER)) {
                success = 0; break;
          } } }
        }  else error(log_err, "intq243oij");  // nocov
      } else if (hi_unbound) {
        if(!inc_lo) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(!((data[i] == NA_INTEGER) || (data[i] > lo_num))) {
                success = 0; break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] > lo_num)) {
                success = 0; break;
          } } }
        } else if (inc_lo) {
          if(na_rm_int) {
            for(i = 0; i < x_len; ++i) {
              if(!((data[i] == NA_INTEGER) || data[i] >= lo_num)) {
                success = 0; break;
            } }
          } else {
            for(i = 0; i < x_len; ++i) {
              if(!(data[i] >= lo_num)) {
                success = 0; break;
          } } }
        }  else error(log_err, "int2945asdf");  // nocov
      } else error(log_err, "inthfg89");  // nocov
    }
  } else if(x_type == STRSXP) {
  // - Strings ---------------------------------------------------------------

    // note that if unbound then the char representation of "Inf"/"-Inf" is
    // never used

    int lo_unbound = (lo_type == REALSXP && REAL(lo)[0] == R_NegInf);
    int hi_unbound = (hi_type == REALSXP && REAL(hi)[0] == R_PosInf);

    const char * lo_chr, * hi_chr;

    // Not ideal to assemble all these SEXPs, but we haven't bothered with an
    // internal interface to strsub given that ends up being relatively
    // complicated.  Also, we really only need this in failure but we're always
    // building it...

    SEXP lo_chr_sxp = PROTECT(asChar(lo));
    SEXP hi_chr_sxp = PROTECT(asChar(hi));
    lo_chr = CHAR(lo_chr_sxp);
    hi_chr = CHAR(hi_chr_sxp);

    SEXP lim = PROTECT(ScalarInteger(12));
    SEXP mark = PROTECT(ScalarLogical(1));

    SEXP hilo_string = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(hilo_string, 0, lo_chr_sxp);
    SET_STRING_ELT(hilo_string, 1, hi_chr_sxp);
    SEXP hilo_string_sub = PROTECT(CSR_strsub(hilo_string, lim, mark));

    lo_as_chr = lo_unbound ?
      CSR_num_as_chr(asReal(lo), 0) :
      CSR_smprintf2(10000L, "\"%s\"", CHAR(STRING_ELT(hilo_string_sub, 0)), "");
    hi_as_chr = hi_unbound ?
      CSR_num_as_chr(asReal(hi), 0) :
      CSR_smprintf2(10000L, "\"%s\"", CHAR(STRING_ELT(hilo_string_sub, 1)), "");

    UNPROTECT(6);

    if(!lo_unbound && !hi_unbound && strcmp(lo_chr, hi_chr) > 0) {
      error(
        "Argument `hi` (%s) must be greater than or equal to `lo` (%s).",
        lo_chr, hi_chr
      );
    }
    if(!lo_unbound && !hi_unbound) {
      if(!inc_lo && !inc_hi) {
        if(na_rm_int) {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                (STRING_ELT(x, i) == NA_STRING) ||
                (
                  strcmp(CHAR(STRING_ELT(x, i)), lo_chr) > 0 &&
                  strcmp(CHAR(STRING_ELT(x, i)), hi_chr) < 0
              ) )
            ) {
              success = 0;
              break;
          } }
        } else {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                strcmp(CHAR(STRING_ELT(x, i)), lo_chr) > 0 &&
                strcmp(CHAR(STRING_ELT(x, i)), hi_chr) < 0 &&
                STRING_ELT(x, i) != NA_STRING
              )
            ) {
              success = 0;
              break;
        } } }
      } else if (inc_lo && inc_hi) {
        if(na_rm_int) {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                (STRING_ELT(x, i) == NA_STRING) ||
                (
                  strcmp(CHAR(STRING_ELT(x, i)), lo_chr) >= 0 &&
                  strcmp(CHAR(STRING_ELT(x, i)), hi_chr) <= 0
              ) )
            ) {
              success = 0;
              break;
          } }
        } else {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                strcmp(CHAR(STRING_ELT(x, i)), lo_chr) >= 0 &&
                strcmp(CHAR(STRING_ELT(x, i)), hi_chr) <= 0 &&
                STRING_ELT(x, i) != NA_STRING
              )
            ) {
              success = 0;
              break;
        } } }
      } else if (inc_lo) {
        if(na_rm_int) {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                (STRING_ELT(x, i) == NA_STRING) ||
                (
                   strcmp(CHAR(STRING_ELT(x, i)), lo_chr) >= 0 &&
                   strcmp(CHAR(STRING_ELT(x, i)), hi_chr) < 0
              ) )
            ) {
              success = 0;
              break;
          } }
        } else {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                strcmp(CHAR(STRING_ELT(x, i)), lo_chr) >= 0 &&
                strcmp(CHAR(STRING_ELT(x, i)), hi_chr) < 0 &&
                STRING_ELT(x, i) != NA_STRING
              )
            ) {
              success = 0;
              break;
        } } }
      } else if (inc_hi) {
        if(na_rm_int) {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                (STRING_ELT(x, i) == NA_STRING) ||
                (
                  strcmp(CHAR(STRING_ELT(x, i)), lo_chr) > 0 &&
                  strcmp(CHAR(STRING_ELT(x, i)), hi_chr) <= 0
              ) )
            ) {
              success = 0;
              break;
          } }
        } else {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                strcmp(CHAR(STRING_ELT(x, i)), lo_chr) > 0 &&
                strcmp(CHAR(STRING_ELT(x, i)), hi_chr) <= 0) &&
                STRING_ELT(x, i) != NA_STRING
            ) {
              success = 0;
              break;
        } } }
      } else error(log_err, "stringq34");  // nocov
    } else if (lo_unbound && hi_unbound) {
      if(na_rm_int) success = 1;
      else {
        for(i = 0; i < x_len; ++i) {
          if(STRING_ELT(x, i) == NA_STRING) {
            success = 0;
            break;
      } } }
    } else if (lo_unbound) {
      if(!inc_hi) {
        if(na_rm_int) {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                (STRING_ELT(x, i) == NA_STRING) ||
                strcmp(CHAR(STRING_ELT(x, i)), hi_chr) < 0
              )
            ) {
              success = 0; break;
          } }
        } else {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                strcmp(CHAR(STRING_ELT(x, i)), hi_chr) < 0 &&
                STRING_ELT(x, i) != NA_STRING)
            ) {
              success = 0; break;
        } } }
      } else if (inc_hi) {
        if(na_rm_int) {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                (STRING_ELT(x, i) == NA_STRING) ||
                strcmp(CHAR(STRING_ELT(x, i)), hi_chr) <= 0)
            ) {
              success = 0; break;
          } }
        } else {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                strcmp(CHAR(STRING_ELT(x, i)), hi_chr) <= 0 &&
                STRING_ELT(x, i) != NA_STRING)
            ) {
              success = 0; break;
        } } }
      }  else error(log_err, "stringq243oij");  // nocov
    } else if (hi_unbound) {
      if(!inc_lo) {
        if(na_rm_int) {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                (STRING_ELT(x, i) == NA_STRING) ||
                (strcmp(CHAR(STRING_ELT(x, i)), lo_chr) > 0)
              )
            ) {
              success = 0; break;
          } }
        } else {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                strcmp(CHAR(STRING_ELT(x, i)), lo_chr) > 0 &&
                STRING_ELT(x, i) != NA_STRING
              )
            ) {
              success = 0; break;
        } } }
      } else if (inc_lo) {
        if(na_rm_int) {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                (STRING_ELT(x, i) == NA_STRING) ||
                strcmp(CHAR(STRING_ELT(x, i)), lo_chr) >= 0
              )
            ) {
              success = 0; break;
          } }
        } else {
          for(i = 0; i < x_len; ++i) {
            if(
              !(
                strcmp(CHAR(STRING_ELT(x, i)), lo_chr) >= 0 &&
                STRING_ELT(x, i) != NA_STRING
              )
            ) {
              success = 0; break;
        } } }
      }  else error(log_err, "string2945asdf");  // nocov
    } else error(log_err, "stringhfg89");  // nocov
  } else {
    error(
      "Argument `x` must be numeric-like or character (is %s).",
      type2char(x_type)
    );
  }
  if(!success) {
    char * msg_val;
    if(x_type == STRSXP) {
      SEXP string_sub = PROTECT(allocVector(STRSXP, 1));
      SET_STRING_ELT(string_sub, 0, STRING_ELT(x, i));
      const char * msg_val_sub = CHAR(
        asChar(
          PROTECT(CSR_strsub(
            string_sub, PROTECT(ScalarInteger(20)),
            PROTECT(ScalarLogical(1))
      ) ) ) );
      UNPROTECT(4);
      msg_val = CSR_smprintf2(10000, "\"%s\"", msg_val_sub, "");
    } else {
      msg_val = CSR_num_as_chr(
        (double)(x_type == REALSXP ?
          REAL(x)[i] :
          // NA_INT doesn't print as NA after coercion to dbl, NA_REAL does
          (INTEGER(x)[i] == NA_INTEGER ? NA_REAL : INTEGER(x)[i] )
        ), 0
      );
    }
    char * msg = CSR_smprintf6(
      10000, "`%s` at index %s not in `%s%s,%s%s`",
      msg_val,
      CSR_len_as_chr(i + 1),
      inc_lo_str, lo_as_chr, hi_as_chr, inc_hi_str
    );
    return mkString(msg);
  } else return ScalarLogical(1);
}
