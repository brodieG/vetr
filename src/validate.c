#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                     SETUP                                    |
|                                                                              |
\* -------------------------------------------------------------------------- */

SEXP VALC_validate ();
SEXP VALC_test(SEXP a, SEXP b);
SEXP VALC_parse(SEXP lang, SEXP var_name);
void VALC_parse_recurse(SEXP lang, SEXP lang_track, SEXP var_name, int eval_as_is);
SEXP VALC_name_sub(SEXP symb, SEXP arg_name);
SEXP VALC_name_sub_ext(SEXP symb, SEXP arg_name);
SEXP VALC_remove_parens(SEXP lang);
void VALC_install_objs();
SEXP VALC_evaluate(SEXP lang, SEXP arg_name, SEXP arg_value);

static const
R_CallMethodDef callMethods[] = {
  {"validate", (DL_FUNC) &VALC_validate, 0},
  {"test", (DL_FUNC) &VALC_test, 2},
  {"name_sub", (DL_FUNC) &VALC_name_sub_ext, 2},
  {"parse", (DL_FUNC) &VALC_parse, 2},
  {"remove_parens", (DL_FUNC) &VALC_remove_parens, 1},
  {"eval_check", (DL_FUNC) &VALC_evaluate, 3},
  {NULL, NULL, 0}
};

void R_init_validate(DllInfo *info)
{
 /* Register the .C and .Call routines.
    No .Fortran() or .External() routines,
    so pass those arrays as NULL.
  */
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  VALC_install_objs();
}
// - Objects We Install Once ---------------------------------------------------

// One question: are static objects not garbage collected?  The examples from
// "Writing R Extensions" don't seem to require the protection of these

static SEXP one_dot = NULL;
static SEXP dep_call = NULL;
static SEXP(*alike)(SEXP,SEXP) = NULL;

void VALC_install_objs() {
    if(one_dot == NULL) one_dot = install(".");
    if(dep_call == NULL) {  // ready to eval `deparse(x)` call, just sub in `x`
      dep_call = allocList(2);
      SETCAR(dep_call, install("deparse"));
      SET_TYPEOF(dep_call, LANGSXP);

      // Need to add a `width` argument, etc...
    }
    // Borrowed from Dirk Eddelbuettel (http://stackoverflow.com/a/20479078/2725969)

    if (alike == NULL)
      alike = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("alike", "ALIKEC_alike_fast");
}

// - Testing Function ----------------------------------------------------------

SEXP VALC_test(SEXP a, SEXP b) {
  Rprintf("%s\n", type2char(TYPEOF(a)));
  Rprintf("%s\n", type2char(TYPEOF(CDR(a))));
  return(R_NilValue);
  //Rprintf("%s\n", CHAR(deparse1WithCutoff(lang)));
}

// - Helper Functions ----------------------------------------------------------
/*
  Don't need paren calls since the parsing already accounted for them
*/
SEXP VALC_remove_parens(SEXP lang) {
  SEXP mode, mode_0 = PROTECT(ScalarInteger(0)), mode_1 = PROTECT(ScalarInteger(1));
  mode = mode_0;

  while(TYPEOF(lang) == LANGSXP) {
    if(!strcmp(CHAR(PRINTNAME(CAR(lang))), "(")) {
      if(length(lang) != 2) {
        error("Logic Error: `(` call with more than one argument; contact maintainer.");
      }
    } else if(!strcmp(CHAR(PRINTNAME(CAR(lang))), ".")) {
      if(length(lang) != 2)
        error("`.(` must be used with only one argument.");
      mode = mode_1;
    } else {
      break;
    }
    lang = CADR(lang);
  }
  SEXP res = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(res, 0, lang);
  SET_VECTOR_ELT(res, 1, mode);
  UNPROTECT(3);
  return(res);
}
SEXP VALC_parse(SEXP lang, SEXP var_name) {
  SEXP lang_cpy, res, res_vec, rem_res;
  int mode;

  lang_cpy = PROTECT(duplicate(lang)); // Must copy since we're going to modify this

  rem_res = PROTECT(VALC_remove_parens(lang_cpy));
  lang_cpy = VECTOR_ELT(rem_res, 0);
  mode = asInteger(VECTOR_ELT(rem_res, 1));

  if(TYPEOF(lang_cpy) != LANGSXP) {
    lang_cpy = VALC_name_sub(lang_cpy, var_name);
    res = PROTECT(ScalarInteger(mode ? 10 : 999));
  } else {
    res = PROTECT(allocList(length(lang_cpy)));
    VALC_parse_recurse(lang_cpy, res, var_name, mode);  // lang_cpy, res, are modified internally
  }
  res_vec = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(res_vec, 0, lang_cpy);
  SET_VECTOR_ELT(res_vec, 1, res);
  UNPROTECT(4);
  return(res_vec);
}
void VALC_parse_recurse(SEXP lang, SEXP lang_track, SEXP var_name, int eval_as_is) {
  /*
  If the object is not a language list, then return it, as part of an R vector
  list.  Otherwise, in a loop, recurse with this function on each element of the
  list, placing each an R vector list that combines this element and an auxillary
  value describing the element into a pair list that replicates in structure the
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

  static int counter = -1;
  int call_type = 999;
  counter++;  // Tracks recursion level, used for debugging

  if(TYPEOF(lang) != LANGSXP) {  // Not a language expression
    error("Logic Error: unexpectedly encountered a non-language object");
  }
  const char * call_symb;

  // Determine if we're dealing with a special code, and if so determine what
  // type and record

  call_symb = CHAR(PRINTNAME(CAR(lang)));
  if(eval_as_is) {
    call_type = 10;
  } else if(!strcmp(call_symb, "&&")) {
    call_type = 1;
  } else if(!strcmp(call_symb, "||")) {
    call_type = 2;
  }
  SETCAR(lang_track, ScalarInteger(call_type));             // Track type of call

  lang = CDR(lang);
  lang_track = CDR(lang_track);

  // Loop through remaining elements of call; if any are calls, recurse, otherwise
  // sub for dots and record as templates (999).  Stuff here shouldn't need to
  // be PROTECTed since it is pointed at but PROTECTED stuff.

  while(lang != R_NilValue) {
    // Remove parens removes parens and `.` calls, and indicates whether a `.`
    // call was encountered through the second value in the return list

    SEXP rem_parens = PROTECT(VALC_remove_parens(CAR(lang)));
    if(!eval_as_is && asInteger(VECTOR_ELT(rem_parens, 1))) {
      eval_as_is = 1;
    } else {
      eval_as_is = 0;
    }
    SETCAR(lang, VECTOR_ELT(rem_parens, 0));
    UNPROTECT(1);

    if(TYPEOF(CAR(lang)) == LANGSXP) {
      SEXP track_car = allocList(length(CAR(lang)));
      SETCAR(lang_track, track_car);
      VALC_parse_recurse(CAR(lang), CAR(lang_track), var_name, eval_as_is);
    } else {
      SETCAR(lang, VALC_name_sub(CAR(lang), var_name));
      SETCAR(lang_track, ScalarInteger(eval_as_is ? 10 : 999));
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


/*
Name replacement, substitutes `.` for argname and multi dots for one dot fewer
*/
SEXP VALC_name_sub(SEXP symb, SEXP arg_name) {
  if(TYPEOF(symb) != SYMSXP){
    return(symb);
  }
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
    if (i == 1) {  // one dot and an arg
      return(arg_name);
    } else if (i == 2) { // Most common multi dot scenario, escaped dot
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
SEXP VALC_name_sub_ext(SEXP symb, SEXP arg_name) {
  return(VALC_name_sub(symb, arg_name));
}

SEXP VALC_evaluate_recurse(SEXP lang, SEXP act_codes, SEXP arg_value) {
  /*
  check act_codes:
    if 1 or 2
      recurse and:
        if return value is TRUE
          and act_code == 2, return TRUE
          and act_code == 1, continue
        if return value is character
          and act_code == 1,
            return
          and act_code == 2,
            record for later return if no TRUEs are met
        if return value is TRUE,
        if with mode set to corresponding value (does it matter)
    if 10, eval as is
      if returns character then return character
      if returns FALSE deparse into something like (`x` does not eval to TRUE`)
    if 999, eval as alike
  */
  int mode;

  if(TYPEOF(act_codes) == LISTSXP) {
    if(TYPEOF(lang) != LANGSXP && TYPEOF(lang) != LISTSXP) {
      error("Logic Error: mismatched language and eval type tracking 1; contact maintainer.");
    }
    if(TYPEOF(CAR(act_codes)) != INTSXP) {
      if(TYPEOF(CAR(lang)) != LANGSXP) {
        error("Logic error: call should have been evaluated at previous level; contact maintainer.");
      }
      if(TYPEOF(CAR(act_codes)) != LISTSXP || TYPEOF(CAAR(act_codes)) != INTSXP) {
        error("Logic error: unexpected act_code structure; contact maintainer");
      }
      mode=asInteger(CAAR(act_codes));
    } else {
      mode=asInteger(CAR(act_codes));
    }
  } else {
    if(TYPEOF(lang) == LANGSXP || TYPEOF(lang) == LISTSXP) {
      error("Logic Error: mismatched language and eval type tracking 2; contact maintainer.");
    }
    mode = asInteger(act_codes);
  }
  if(mode == 1 || mode == 2) {
    // Dealing with && or ||, so recurse on each element

    if(TYPEOF(lang) == LANGSXP) {
      int parse_count = 0;
      SEXP err_list = PROTECT(allocList(0)); // Track errors
      lang = CDR(lang);
      act_codes = CDR(act_codes);

      while(lang != R_NilValue) {
        SEXP eval_res;
        eval_res = PROTECT(VALC_evaluate_recurse(CAR(lang), CAR(act_codes), arg_value));
        if(TYPEOF(eval_res) == LISTSXP) {
          if(mode == 1) {// At least one non-TRUE result, which is a failure, so return
            UNPROTECT(2);
            return(eval_res);
          }
          if(mode == 2)  {// All need to fail, so store errors for now
            err_list = listAppend(err_list, eval_res);
          }
        } else if (TYPEOF(eval_res) == LGLSXP && XLENGTH(eval_res) == 1) {
          if(mode == 2) {
            UNPROTECT(2);
            return(eval_res); // At least one TRUE value in or mode
          }
        } else {
          error("Logic Error: unexpected return value when recursively evaluating parse; contact maintainer.");
        }
        lang = CDR(lang);
        act_codes = CDR(act_codes);
        parse_count++;
        UNPROTECT(1);
      }
      if(parse_count != 2)
        error("Logic Error: unexpected language structure for modes 1/2; contact maintainer.");
      UNPROTECT(1);
      if(mode == 2) {  // Only way to get here is if none of previous actually returned TRUE
        return(err_list);
      }
    } else {
      error("Logic Error: in mode c(1, 2), but not a language object; contact maintainer.");
    }
  } else if(mode == 10 || mode == 999) {
    // For all this stuff, need to think about error handling
    // Need environment to eval this in

    SEXP eval_res;

    if(mode == 10) {
      eval_res = PROTECT(eval(lang, R_GlobalEnv));
    } else {
      eval_res = PROTECT(alike(eval(lang, R_GlobalEnv), arg_value));
    }
    if(TYPEOF(eval_res) != LGLSXP || xlength(eval_res) != 1 || !asLogical(eval_res)) {
      SEXP err_msg = PROTECT(allocList(1));
      if(mode == 10) {
        SETCADR(dep_call, lang); // This needs to become a proper message, not just the deparsed call
        SETCAR(err_msg, eval(dep_call, R_GlobalEnv));     // R_GlobalEnv not right place to eval
      } else {
        SETCAR(err_msg, eval_res);     // R_GlobalEnv not right place to eval
      }
      UNPROTECT(2);
      return(err_msg);
    }
    UNPROTECT(1);
    return(eval_res);  // this should be `TRUE`
  } else {
    error("Logic Error: unexpected parse mode %d", mode);
  }
  error("Logic Error: should never get here");
  return(R_NilValue);  // Should never get here
}
/*
TBD if this should call `VALC_parse` directly or not
*/
SEXP VALC_evaluate(SEXP lang, SEXP arg_name, SEXP arg_value) {
  SEXP lang_parsed = PROTECT(VALC_parse(lang, arg_name));
  SEXP res = PROTECT(
    VALC_evaluate_recurse(
      VECTOR_ELT(lang_parsed, 0),
      VECTOR_ELT(lang_parsed, 1),
      arg_value
  ) );
  UNPROTECT(2);  // This seems a bit stupid, PROTECT/UNPROTECT
  return(res);
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
