#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                     SETUP                                    |
|                                                                              |
\* -------------------------------------------------------------------------- */

SEXP VALC_validate ();
SEXP VALC_test(SEXP obj1);

static const
R_CallMethodDef callMethods[] = {
  {"validate", (DL_FUNC) &VALC_validate, 0},
  {"test", (DL_FUNC) &VALC_test, 1},
  {NULL, NULL, 0}
};

void R_init_validate(DllInfo *info)
{
 /* Register the .C and .Call routines.
    No .Fortran() or .External() routines,
    so pass those arrays as NULL.
  */
  R_registerRoutines(info,
  NULL, callMethods,
  NULL, NULL);
}
// - Helper Functions ----------------------------------------------------------

/*
Returns a character pointer to the string representation of the integer; allocates
with R_alloc so in theory don't need to worry about freeing memory


const char * ALIKEC_xlen_to_char(R_xlen_t a) {
  if(a < 0)
    error("Logic Error: unexpected negative length value.");
  int int_len = (int) ceil(log10(a + 1.00001));  // + 1.00001 to account for 0
  char * res;
  res = R_alloc(int_len + 1, sizeof(char));
  sprintf(res, "%td", a);    // Correct type of R_xlen_t?
  return (const char *) res;
}
/* Returns a character pointer containing the results of using `a` as the parent
string and all the others a substrings with `sprintf`

note:
- will over-allocate by up to 8 characters to account for possibility there may
  not be an "%s" in `a`


const char * ALIKEC_sprintf(char * a, const char * b, const char * c, const char * d, const char * e) {
  int full_len = strlen(a) + strlen(b) + strlen(c) + strlen(d) + strlen(e) + 1;
  char * res;
  res = R_alloc(full_len, sizeof(char));
  sprintf(res, a, b, c, d, e);
  return res;
}

/* Estimate how many characters an integer can be represented with

int ALIKEC_int_charlen (R_xlen_t a) {
  if(a < 0)
    error("Logic Error: unexpected negative length value.");
  return (int) ceil(log10(a + 1.1));
}
*/

// - Testing Function ----------------------------------------------------------

SEXP VALC_test(SEXP lang) {
  /*
  If the object is not a language list, then return it, as part of an R vector
  list.  Otherwise, in a loop, recurse with this function on each element of the
  list, placing each an R vector list that combines this element and an auxillary
  value describing the elemnt into a pair list that replicates in structure the
  original language list
  */

  // Don't need paren calls since the parsing already accounted for them
  static int counter = -1;
  counter++;
  SEXP lang_cpy = lang;
  while(
    TYPEOF(lang_cpy) == 6 &&
    !strcmp(CHAR(PRINTNAME(CAR(lang_cpy))), "(")
  ) {
    lang_cpy = CADR(lang_cpy);
  }
  if(TYPEOF(lang_cpy) != 6) {  // Not a language expression
    counter--;
    // Rprintf("Returning Terminal ");
    // PrintValue(lang_cpy);
    return(lang_cpy);
  }
  // Maybe we can avoid computing length of `lang` right here since we're going
  // to loop through it anyway, but then need to figure out how to make a linked
  // list element by element...

  SEXP res, res_cpy, res_vec;
  res = res_cpy = PROTECT(allocList(length(lang_cpy)));  // one more for full call

  // Note, this loop runs one extra time

  int first_time=1;

  while(res != R_NilValue) {
    SEXP rec_val;
    char * call_symb;
    int call_type = 999;

    res_vec=PROTECT(allocVector(VECSXP, 2));
    if(first_time) {
      rec_val=PROTECT(lang_cpy); //unnecessary PROTECT keeps stack balance
    } else {
      rec_val=PROTECT(VALC_test(CAR(lang_cpy)));
    }
    if(TYPEOF(rec_val) == 6) {
      call_symb = CHAR(PRINTNAME(CAR(rec_val)));
      if(!strcmp(call_symb, "&&") || !strcmp(call_symb, "||")) {
        call_type = 1;
    } }
    SET_VECTOR_ELT(res_vec, 0, rec_val);
    SET_VECTOR_ELT(res_vec, 1, PROTECT(ScalarInteger(call_type)));
    SETCAR(res, res_vec);

    UNPROTECT(3);
    lang_cpy = CDR(lang_cpy);
    res = CDR(res);
    first_time = 0;
  }
  UNPROTECT(1);
  counter--;
  return(res_cpy);
}



/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                     TYPE                                     |
|                                                                              |
\* -------------------------------------------------------------------------- */

/*
compare types, accounting for "integer like" numerics; empty string means success,
otherwise outputs an a character string explaining why the types are not alike
*/

SEXP VALC_validate() {
  return R_NilValue;
}
