#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                     SETUP                                    |
|                                                                              |
\* -------------------------------------------------------------------------- */

SEXP VALC_validate();
SEXP VALC_test(SEXP a, SEXP b);
SEXP VALC_test1(SEXP a);
SEXP VALC_test2(SEXP a);
void VALC_stop(SEXP call, const char * msg);
SEXP VALC_parse(SEXP lang, SEXP var_name);
void VALC_parse_recurse(SEXP lang, SEXP lang_track, SEXP var_name, int eval_as_is);
SEXP VALC_name_sub(SEXP symb, SEXP arg_name);
SEXP VALC_name_sub_ext(SEXP symb, SEXP arg_name);
SEXP VALC_remove_parens(SEXP lang);
void VALC_install_objs();
SEXP VALC_evaluate(SEXP lang, SEXP arg_name, SEXP arg_value, SEXP rho);

// - Objects We Install Once ---------------------------------------------------

// One question: are static objects not garbage collected?  The examples from
// "Writing R Extensions" don't seem to require the protection of these

SEXP VALC_SYM_one_dot;
SEXP VALC_SYM_deparse;
SEXP VALC_SYM_quote;
SEXP VALC_TRUE;
SEXP(*VALC_alike)(SEXP,SEXP);
SEXP(*VALC_match_call)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
SEXP(*VALC_get_frame_data)(SEXP,SEXP,SEXP,int);
SEXP(*VALC_get_fun)(SEXP,SEXP);

static const
R_CallMethodDef callMethods[] = {
  {"validate", (DL_FUNC) &VALC_validate, 3},
  {"test", (DL_FUNC) &VALC_test, 2},
  {"name_sub", (DL_FUNC) &VALC_name_sub_ext, 2},
  {"parse", (DL_FUNC) &VALC_parse, 2},
  {"remove_parens", (DL_FUNC) &VALC_remove_parens, 1},
  {"eval_check", (DL_FUNC) &VALC_evaluate, 4},
  {"test1", (DL_FUNC) &VALC_test1, 1},
  {"test2", (DL_FUNC) &VALC_test2, 1},
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
  VALC_TRUE = ScalarLogical(1);
  VALC_match_call = (SEXP(*)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP)) R_GetCCallable("matchcall", "MC_match_call_internal");
  VALC_alike = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("alike", "ALIKEC_alike_fast");
  VALC_get_frame_data = (SEXP(*)(SEXP,SEXP,SEXP,int)) R_GetCCallable("matchcall", "MC_get_frame_data");
  VALC_get_fun = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("matchcall", "MC_get_fun");
}
// - Testing Function ----------------------------------------------------------

SEXP VALC_test(SEXP a, SEXP b) {
  SEXP c;
  // c = PROTECT(allocVector(VECSXP, 3));
  // SET_VECTOR_ELT(c, 0, R_NilValue);
  // SET_VECTOR_ELT(c, 1, R_NilValue);
  // SET_VECTOR_ELT(c, 2, R_NilValue);
  // VECTOR_ELT(c, 0);
  // VECTOR_ELT(c, 1);
  // VECTOR_ELT(c, 2);
  // UNPROTECT(1);
  c = PROTECT(list3(R_NilValue, R_NilValue, R_NilValue));
  CAR(c);
  CADR(c);
  CADDR(c);
  UNPROTECT(1);
  return(R_NilValue);
}

SEXP VALC_test1(SEXP a) {
  SEXP b=install("quote");
  return(a);
}
SEXP VALC_test2(SEXP a) {
  SEXP b=VALC_SYM_quote;
  return(a);
}
// - Helper Functions ----------------------------------------------------------

int IS_TRUE(SEXP x) {
  return(TYPEOF(x) == LGLSXP && XLENGTH(x) == 1 && asLogical(x));
}
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
/*
Deparse R expression to const char

if first_only then only return first line
*/
const char * VALC_deparse(SEXP call, int first_only) {
  // SEXP quot_call = PROTECT(allocList(2));
  // SEXP dep_call = PROTECT(allocList(2));
  // SETCAR(quot_call, VALC_SYM_quote);
  // SETCADR(quot_call, call);
  // SET_TYPEOF(quot_call, LANGSXP);
  // SETCAR(dep_call, VALC_SYM_deparse);
  // SETCADR(dep_call, quot_call);
  // SET_TYPEOF(dep_call, LANGSXP);

  // SEXP err_vec = eval(dep_call, R_GlobalEnv);

  // if(first_only || XLENGTH(err_vec) == (R_xlen_t) 1) {
  //   return(CHAR(STRING_ELT(err_vec, 1)));
  // } else {
  //   // Get size of deparsed expression

  //   const R_xlen_t err_len = XLENGTH(err_vec);


  // }

}
/*
Fake `stop`

Main benefit is that it allows us to control the call that gets displayed.
*/
void VALC_stop(SEXP call, const char * msg) {
  SEXP quot_call = list2(VALC_SYM_quote, call);
  SET_TYPEOF(quot_call, LANGSXP);
  SEXP cond_call = PROTECT(
    list3(install("simpleError"), ScalarString(mkChar(msg)), quot_call)
  );
  SET_TYPEOF(cond_call, LANGSXP);
  SEXP cond = PROTECT(eval(cond_call, R_GlobalEnv));
  SEXP err_call = PROTECT(list2(install("stop"), cond));
  SET_TYPEOF(err_call, LANGSXP);
  UNPROTECT(3);
  eval(err_call, R_GlobalEnv);
  error("Logic Error: should never get here; contact maintainer.");
}
/*
Creat simple error for a tag
*/
void VALC_arg_error(SEXP tag, SEXP fun_call, const char * err_base) {
  const char * err_tag = CHAR(asChar(tag));
  char * err_msg = R_alloc(
    strlen(err_base) - 2 + strlen(err_tag) + 1, sizeof(char)
  );
  sprintf(err_msg, err_base, err_tag);
  VALC_stop(fun_call, err_msg);
  error("Logic Error: shouldn't get here 181; contact maintainer.");
}
/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                    PARSE                                     |
|                                                                              |
\* -------------------------------------------------------------------------- */

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
      return(VALC_SYM_one_dot);
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
/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                   EVAL                                       |
|                                                                              |
\* -------------------------------------------------------------------------- */

SEXP VALC_evaluate_recurse(SEXP lang, SEXP act_codes, SEXP arg_value, SEXP rho) {
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
      SEXP eval_res;
      lang = CDR(lang);
      act_codes = CDR(act_codes);

      while(lang != R_NilValue) {
        eval_res = PROTECT(VALC_evaluate_recurse(CAR(lang), CAR(act_codes), arg_value, rho));
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
      if(mode == 2) {  // Only way to get here is if none of previous actually returned TRUE and mode is OR
        return(err_list);
      }
      return(eval_res);  // Or if all returned TRUE and mode is AND
    } else {
      error("Logic Error: in mode c(1, 2), but not a language object; contact maintainer.");
    }
  } else if(mode == 10 || mode == 999) {
    // For all this stuff, need to think about error handling

    SEXP eval_res;

    if(mode == 10) {
      eval_res = PROTECT(eval(lang, rho));
    } else {
      eval_res = PROTECT(VALC_alike(eval(lang, rho), arg_value));
    }
    if(TYPEOF(eval_res) != LGLSXP || xlength(eval_res) != 1 || !asLogical(eval_res)) {
      SEXP err_msg = PROTECT(allocList(1));
      if(mode == 10) {
        // Tried to do this as part of init originally so we don't have to repeat
        // wholesale creation of call, but the call elements kept getting over
        // written by other stuff.  Not sure how to protect in calls defined at
        // top level

        SEXP dep_call = PROTECT(allocList(2));
        SETCAR(dep_call, VALC_SYM_deparse);
        SEXP quot_call = PROTECT(allocList(2));
        SETCAR(quot_call, VALC_SYM_quote);
        SETCADR(quot_call, lang);
        SET_TYPEOF(quot_call, LANGSXP);
        SETCADR(dep_call, quot_call);
        SET_TYPEOF(dep_call, LANGSXP);
        SETCAR(err_msg, eval(dep_call, rho));  // Could span multiple lines...; need to address
        UNPROTECT(2);
      } else {
        SETCAR(err_msg, eval_res);
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
SEXP VALC_evaluate(SEXP lang, SEXP arg_name, SEXP arg_value, SEXP rho) {
  if(TYPEOF(arg_name) != SYMSXP)
    error("Argument `arg_name` must be a symbol.");
  if(TYPEOF(rho) != ENVSXP)
    error("Argument `rho` must be an environment.");
  SEXP lang_parsed = PROTECT(VALC_parse(lang, arg_name));
  SEXP res = PROTECT(
    VALC_evaluate_recurse(
      VECTOR_ELT(lang_parsed, 0),
      VECTOR_ELT(lang_parsed, 1),
      arg_value,
      rho
  ) );
  UNPROTECT(2);  // This seems a bit stupid, PROTECT/UNPROTECT
  return(res);
}
/* -------------------------------------------------------------------------- *\
|                                                                              |
|                                    CHECK                                     |
|                                                                              |
\* -------------------------------------------------------------------------- */

SEXP VALC_validate(SEXP sys_frames, SEXP sys_calls, SEXP sys_pars) {
  SEXP R_TRUE = PROTECT(ScalarLogical(1)),
    chr_exp = PROTECT(ScalarString(mkChar("expand"))),
    one=PROTECT(ScalarInteger(1)), zero=PROTECT(ScalarInteger(0));

  // Get calls from function to validate and validator call

  SEXP fun_call = PROTECT(
    CAR(
      VALC_match_call(
        chr_exp, R_TRUE, R_TRUE, R_TRUE, one, R_NilValue, sys_frames, sys_calls,
        sys_pars
  ) ) );
  // Get definition of fun in original call; this unfortunately requires repeating
  // some of the logic in the step above, but is pretty fast

  SEXP fun_frame_dat = PROTECT(
    VALC_get_frame_data(sys_frames, sys_calls, sys_pars, 1)
  );
  SEXP fun_dyn_par_frame = CADR(fun_frame_dat);
  SEXP fun_frame = CADDR(fun_frame_dat);
  SEXP fun = PROTECT(
    VALC_get_fun(fun_dyn_par_frame, CAR(fun_frame_dat))
  );
  // Now get matching call (could save up to 1.9us if we used different method,
  // but nice thing of doing it this way is this is guaranteed to match to
  // fun_call)

  SEXP val_call_data = PROTECT(
    VALC_match_call(
      chr_exp, R_TRUE, R_TRUE, R_TRUE, zero, fun, sys_frames, sys_calls,
      sys_pars
  ) );
  SEXP val_call = CAR(val_call_data);
  SEXP val_call_types = CADR(val_call_data);

  // For the elements with validation call setup, check for errors;  Note that
  // we need to skip the first element of the calls since we only care about the
  // args.

  SEXP val_call_cpy, fun_call_cpy, val_call_types_cpy;

  for(
    val_call_cpy = CDR(val_call), fun_call_cpy = CDR(fun_call),
    val_call_types_cpy = val_call_types;
    val_call_cpy != R_NilValue;
    val_call_cpy = CDR(val_call_cpy), fun_call_cpy = CDR(fun_call_cpy),
    val_call_types_cpy = CDR(val_call_types_cpy)
  ) {
    if(TAG(val_call_cpy) != TAG(fun_call_cpy))
      error("Logic Error: tag mismatch between function and validation; contact maintainer.");
    SEXP val_tok, fun_tok;
    val_tok = CAR(val_call_cpy);
    if(val_tok == R_MissingArg) continue;
    fun_tok = CAR(fun_call_cpy);
    if(fun_tok == R_MissingArg)
      VALC_arg_error(TAG(fun_call_cpy), fun_call, "Argument `%s` is missing");
    // Need to evaluate the argument

    SEXP eval_env;

    switch(asInteger(CAR(val_call_types_cpy))) {
      case 1: eval_env = fun_dyn_par_frame; break;  // user args evaled in parent
      case 2: eval_env = fun_frame; break;          // def args evaled in fun
      default:
        error("Logic Error: unexpected arg type %d", asInteger(CAR(val_call_types_cpy)));
    }
    int err_val = 0;
    int * err_point = &err_val;

    SEXP fun_val = R_tryEval(fun_tok, fun_dyn_par_frame, err_point);
    if(* err_point) {
      VALC_arg_error(
        TAG(fun_call_cpy), fun_call,
        "Argument `%s` produced error during evaluation; see prior error."
    );}
    // Evaluate the validation expression

    SEXP val_res = VALC_evaluate(val_tok, TAG(val_call_cpy), fun_val, fun_frame);
    if(IS_TRUE(val_res)) continue;  // success, check next
    else if(TYPEOF(val_res) != LISTSXP)
      error(
        "Logic Error: unexpected type %s when evaluating test for %s; contact mainainer.",
        type2char(TYPEOF(val_res)), CHAR(asChar(TAG(fun_call_cpy)))
      );
    // - Failure ---------------------------------------------------------------

    // Failure, explain why; two pass process because we first need to determine
    // size of error, allocate, then convert to char

    SEXP val_res_cpy;
    const char * err_sub_base = "Pass all of the following:\n";
    int count_top = 0, count_sub = 0, size = 0;
    const int err_sub_base_len = strlen(err_sub_base);

    // First pass get sizes

    for(
      val_res_cpy = val_res; val_res_cpy != R_NilValue;
      val_res_cpy = CDR(val_res_cpy)
    ) {
      SEXP err_vec = CAR(val_res_cpy);
      if(TYPEOF(err_vec) != STRSXP)
        error("Logic Error: did not get character err msg; contact maintainer");

      const R_xlen_t err_items = xlength(err_vec);
      R_xlen_t i;

      count_top++;
      count_sub += err_items > 1 ? err_items : 0;

      for(i = 0; i < err_items; i++) {
        size += strlen(CHAR(STRING_ELT(err_vec, i)));
      }
      if(err_items > 1) size += err_sub_base_len;
    }
    // Depending on whether there is one error or multiple ones (multiple means
    // value failed to match any of the OR possibilities), render error

    const char * err_arg = CHAR(asChar(TAG(fun_call_cpy)));

    if(count_top == 1) {
      const char * err_base = "Argument `%s` fails validation: %s";
      const char * err_msg = CHAR(asChar(CAR(val_res)));
      char * err_full = R_alloc(
        (strlen(err_base) - 4) + strlen(err_arg) + strlen(err_msg) + 1,
        sizeof(char)
      );
      sprintf(err_full, err_base, err_arg, err_msg);
      VALC_stop(fun_call, err_full);
      error("Logic Error: shouldn't get here, R error should've been thrown; contact maintainer.");
    } else if (count_top > 1) {
      const char * err_base = "Argument `%s` fails all of the following:\n";

      size += strlen(err_base) + strlen(err_arg) + 5 * count_top + 7 * count_sub;
      /*
      SAMPLE ERROR:

      Argument `n` failed because it did not meet any of the following requirements:
        - Type mismatch, expected "logical", but got "integer"
        - Length mismatch, expected 2 but got 3
        - !any(is.na(x))
        - pass all of the following:
          + Modes: numeric, character
          + Lengths: 3, 4
          + target is numeric, current is character
        - attribute `dim` is missing

      Now that we know the size of the error we can construct it
      */

      char * err_final = R_alloc(size + 1, sizeof(char));
      char * err_final_cpy = err_final;

      sprintf(err_final_cpy, err_base, err_arg);
      err_final_cpy = err_final_cpy + strlen(err_final_cpy);

      // Second pass construct string

      for(
        val_res_cpy = val_res; val_res_cpy != R_NilValue;
        val_res_cpy = CDR(val_res_cpy)
      ) {
        SEXP err_vec = CAR(val_res_cpy);
        const R_xlen_t err_items = xlength(err_vec);
        R_xlen_t i;

        if(err_items > 1) {
          sprintf(err_final_cpy, "  - %s\n", err_sub_base);
          err_final_cpy += err_sub_base_len + 5;

          for(i = 0; i < err_items; i ++) {
            const char * err_sub = CHAR(STRING_ELT(err_vec, i));
            sprintf(err_final_cpy, "    + %s\n", err_sub);
            err_final_cpy += strlen(err_sub) + 7;
          }
        } else {
          const char * err_sub = CHAR(STRING_ELT(err_vec, 0));
          sprintf(err_final_cpy, "  - %s\n", err_sub);
          err_final_cpy += strlen(err_sub) + 5;
        }
      }
      VALC_stop(fun_call, err_final);
      error("Logic Error: should never get here 629; contact maintainer.");
    } else {
      error("Logic Error: no values in result error; contact maintainer.");
    }
  }
  if(val_call_cpy != R_NilValue || fun_call_cpy != R_NilValue)
    error("Logic Error: fun and validation matched calls different lengths; contact maintainer.");

  // Match the calls up
  UNPROTECT(4);
  UNPROTECT(4); // SEXPs used as arguments for match_call
  return VALC_TRUE;
}
