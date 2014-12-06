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

SEXP VALC_test(SEXP obj1) {

  if(TYPEOF(obj1) != 6) {  // Not a language expression
    PrintValue(obj1);
    return(obj1);
  }
  SEXP obj1_orig = obj1;   // Keep a copy of pointer to beginning of object
  while(obj1 != R_NilValue) {
    SETCAR(obj1, VALC_test(CAR(obj1)));
    obj1 = CDR(obj1);
  }
  return(obj1_orig);
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
