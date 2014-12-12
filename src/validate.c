#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                     SETUP                                    |
|                                                                              |
\* -------------------------------------------------------------------------- */

SEXP VALC_validate ();
SEXP VALC_test(SEXP lang);
SEXP VALC_parse(SEXP lang, SEXP var_name);
void VALC_parse_recurse(SEXP lang, SEXP lang_track, SEXP var_name);
SEXP VALC_name_sub(SEXP symb, SEXP arg_name, int mode);
SEXP VALC_name_sub_ext(SEXP symb, SEXP arg_name, SEXP mode);

static const
R_CallMethodDef callMethods[] = {
  {"validate", (DL_FUNC) &VALC_validate, 0},
  {"test", (DL_FUNC) &VALC_test, 1},
  {"name_sub", (DL_FUNC) &VALC_name_sub_ext, 3},
  {"parse", (DL_FUNC) &VALC_parse, 2},
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
  Don't need paren calls since the parsing already accounted for them
*/
SEXP VALC_remove_parens(SEXP lang) {
  while(
    TYPEOF(lang) == LANGSXP &&
    !strcmp(CHAR(PRINTNAME(CAR(lang))), "(")
  ) {
    lang = CADR(lang);
  }
  return(lang);
}

// - Testing Function ----------------------------------------------------------

SEXP VALC_test(SEXP lang) {
  return(VALC_parse(lang, install("test")));
}
SEXP VALC_parse(SEXP lang, SEXP var_name) {
  SEXP lang_cpy, res, res_vec;

  lang_cpy = PROTECT(duplicate(lang)); // Must copy since we're going to modify this
  lang_cpy = PROTECT(VALC_remove_parens(lang_cpy));

  if(TYPEOF(lang_cpy) != LANGSXP) {
    lang_cpy = PROTECT(VALC_name_sub(lang_cpy, var_name, 2)); // unnecessary PROTECT for stack balance
    res = ScalarInteger(999);
  } else {
    res = PROTECT(allocList(length(lang_cpy)));
    VALC_parse_recurse(lang_cpy, res, var_name);  // lang_cpy, res, are modified internally
  }
  res_vec = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(res_vec, 0, lang_cpy);
  SET_VECTOR_ELT(res_vec, 1, res);
  UNPROTECT(4);
  return(res_vec);
}
void VALC_parse_recurse(SEXP lang, SEXP lang_track, SEXP var_name) {
  /*
  If the object is not a language list, then return it, as part of an R vector
  list.  Otherwise, in a loop, recurse with this function on each element of the
  list, placing each an R vector list that combines this element and an auxillary
  value describing the elemnt into a pair list that replicates in structure the
  original language list.

  Note we're purposefully modifying calls by reference so that the top level
  calls reflect the full substitution of the parse process.

  We want to modify the copy of the original call as well, need to dig
  into list with call to do so; maybe should use parallel trees, one with
  the expressions, one with the status, and the recursion is done by
  passing pointers to the current position in each tree.  Would be simpler.
  Here instead we try to cram both trees into one by using lists and we
  get this mess below

  */

  static int counter = -1, call_type = 999;
  counter++;  // Tracks recursion level, used for debugging

  if(TYPEOF(lang) != LANGSXP) {  // Not a language expression
    error("Logic Error: unexpectedly encountered a non-language object");
  }
  const char * call_symb;

  // Determine if we're dealing with a special code, and if so determine what
  // type and record

  PrintValue(CAR(lang));
  call_symb = CHAR(PRINTNAME(CAR(lang)));
  if(!strcmp(call_symb, "&&")) {
    call_type = 1;
  } else if(!strcmp(call_symb, "||")) {
    call_type = 2;
  } else if(!strcmp(call_symb, ".")) {
    call_type = 10;
  }
  SETCAR(lang, VALC_name_sub(CAR(lang), var_name, 1));  // Subs `.` if needed
  SETCAR(lang_track, ScalarInteger(call_type));             // Track type of call

  lang = CDR(lang);
  lang_track = CDR(lang_track);

  // Loop through remaining elements of call; if any are calls, recurse, otherwise
  // sub for dots and record as templates (999).  Stuff here shouldn't need to
  // be PROTECTed since it is pointed at but PROTECTED stuff.

  while(lang != R_NilValue) {
    SETCAR(lang, VALC_remove_parens(CAR(lang)));  // Note, this is done by reference

    if(TYPEOF(CAR(lang)) == LANGSXP) {
      SEXP track_car = allocList(length(CAR(lang)));
      SETCAR(lang_track, track_car);
      VALC_parse_recurse(CAR(lang), CAR(lang_track), var_name);
    } else {
      SETCAR(lang, VALC_name_sub(CAR(lang), var_name, 2));
      SETCAR(lang_track, ScalarInteger(999));
    }
    lang = CDR(lang);
    lang_track = CDR(lang_track);
  }
  if(lang == R_NilValue && lang_track != R_NilValue) {
    error("Logic Error: unsychronized call tree and call tracking tree; contact maintainer.");
  }
  counter--;

  // Don't return anything as all is done by modifying `lang` and `lang_track`
}

static SEXP identity = NULL;  // Same as R_NilValue?
static SEXP one_dot = NULL;

/*
Name replacement, substitutes:

* If a called function (mode == 1)
    * If a dot, replace with `identity`
    * If only dots but more than one dot, replace with one dot fewer
* If a normal symbol (mode == 2)
    * If a dot, replace with arg name
    * If only dots but more than one, replace with one dot fewer
*/
SEXP VALC_name_sub(SEXP symb, SEXP arg_name, int mode) {
  if(mode != 1 && mode != 2)
    error("Logic error: invalid `mode` value in internal fun; contact maintainer.");
  if(TYPEOF(symb) != SYMSXP){
    return(symb);
  }
  Rprintf("Type: %s\n", type2char(TYPEOF(symb)));
  const char * symb_char = CHAR(PRINTNAME(symb));  // this comes out as const, but we use it as non-const, could this cause problems?

  int i = 0, non_dot = 0;

  while(symb_char[i]) {
    if(symb_char[i] != '.') {
      non_dot = 1;
      break;
    }
    i++;
    if(i > 15000)
      error("Logic Error: unexpectedly large symbol name (>15000, shouldn't happen); contact maintainer.");
  }
  if(!non_dot && i > 0) {  // Name is only dots, and at least one
    if(i == 1 && mode == 1) {  // One dot and a fun
      if(identity == NULL) identity = install("identity");
      return(identity);
    } else if (i == 1 && mode == 2) {  // one dot and an arg
      return(arg_name);
    } else if (i == 2) { // Most common multi dot scenario, escaped dot
      if(one_dot == NULL) one_dot = install(".");
      return(one_dot);
    } else {
      // Need to remove one dot

      size_t name_len;
      name_len = strlen(symb_char);
      char * symb_char_cpy;
      symb_char_cpy = R_alloc(name_len, sizeof(char));  // Could allocate one less than this
      strcpy(symb_char_cpy, symb_char);                 // copy to make non const
      symb_char_cpy[i - 1] = '\0';                      // shorten by one
      return(install(symb_char_cpy));
  } }
  return(symb);
}
/*
Unit testing interface
*/
SEXP VALC_name_sub_ext(SEXP symb, SEXP arg_name, SEXP mode) {
  return(VALC_name_sub(symb, arg_name, asInteger(mode)));
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
