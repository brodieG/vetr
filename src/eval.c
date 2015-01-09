#include "validate.h"

/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */

SEXP VALC_evaluate_recurse(
  SEXP lang, SEXP act_codes, SEXP arg_value, SEXP arg_name, SEXP lang_full,
  SEXP rho
) {
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
        eval_res = PROTECT(
          VALC_evaluate_recurse(
            CAR(lang), CAR(act_codes), arg_value, arg_name, lang_full, rho
        ) );
        if(TYPEOF(eval_res) == LISTSXP) {
          if(mode == 1) {// At least one non-TRUE result, which is a failure, so return
            UNPROTECT(2);
            return(eval_res);
          }
          if(mode == 2)  {// All need to fail, so store errors for now
            err_list = listAppend(err_list, eval_res);
          }
        } else if (VALC_all(eval_res) > 0) {
          if(mode == 2) {
            UNPROTECT(2);
            return(ScalarLogical(1)); // At least one TRUE value in or mode
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
    SEXP eval_res, eval_tmp;
    int err_val = 0, eval_res_c;
    int * err_point = &err_val;
    eval_tmp = PROTECT(R_tryEval(lang, rho, err_point));
    if(* err_point) {
      VALC_arg_error(
        arg_name, lang_full,
        "Validation expression for argument `%s` produced an error (see previous error)."
      );
    }
    if(mode == 10) {
      eval_res_c = VALC_all(eval_tmp);
      eval_res = PROTECT(ScalarLogical(eval_res_c == 1));
    } else {
      eval_res = PROTECT(VALC_alike(eval_tmp, arg_value));
    }
    // Note we're handling both user exp and template eval here

    if(
      TYPEOF(eval_res) != LGLSXP || XLENGTH(eval_res) != 1 ||
      !asLogical(eval_res)
    ) {
      SEXP err_msg = PROTECT(allocList(1));
      // mode == 10 is user eval, special treatment to produce err msg

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

        const char * err_call = CHAR(STRING_ELT(eval(dep_call, rho), 0));  // Could span multiple lines...; need to address
        char * err_tok;
        switch(eval_res_c) {
          case -2: {
              const char * err_tok_tmp = type2char(TYPEOF(eval_tmp));
              const char * err_tok_base = "a %s instead of a logical";
              err_tok = R_alloc(
                strlen(err_tok_tmp) + strlen(err_tok_base), sizeof(char)
              );
              if(!sprintf(err_tok, err_tok_base, err_tok_tmp))
                error("Logic error: could not build token error; contact maintainer");
            }
            break;
          case -1: err_tok = "FALSE"; break;
          case 0: err_tok = "a logical with non-TRUE values"; break;
          default:
            error("Logic Error: unexpected user exp eval value; contact maintainer.");
        }
        const char * err_base = "`%s` %s %s";
        const char * err_act = TYPEOF(lang) == LANGSXP ? "is" : "is";  // used to want verb flexibility, leaving it in just in case
        char * err_str = R_alloc(
          strlen(err_call) + strlen(err_base) + strlen(err_act) + strlen(err_tok),
          sizeof(char)
        );
        if(!sprintf(err_str, err_base, err_call, err_act, err_tok))
          error("Logic Error: could not construct error message; contact maintainer.");
        SETCAR(err_msg, mkString(err_str));
        UNPROTECT(2);
      } else {
        SETCAR(err_msg, eval_res);
      }
      UNPROTECT(3);
      return(err_msg);
    }
    UNPROTECT(2);
    return(eval_res);  // this should be `TRUE`
  } else {
    error("Logic Error: unexpected parse mode %d", mode);
  }
  error("Logic Error: should never get here");
  return(R_NilValue);  // Should never get here
}
/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */
/*
TBD if this should call `VALC_parse` directly or not
*/
SEXP VALC_evaluate(
  SEXP lang, SEXP arg_name, SEXP arg_value, SEXP lang_full, SEXP rho
) {
  if(TYPEOF(arg_name) != SYMSXP)
    error("Argument `arg_name` must be a symbol.");
  if(TYPEOF(rho) != ENVSXP)
    error("Argument `rho` must be an environment.");
  SEXP lang_parsed = PROTECT(VALC_parse(lang, arg_name, rho));
  SEXP res = PROTECT(
    VALC_evaluate_recurse(
      VECTOR_ELT(lang_parsed, 0),
      VECTOR_ELT(lang_parsed, 1),
      arg_value, arg_name, lang_full, rho
  ) );
  // Remove duplicates, if any

  switch(TYPEOF(res)) {
    case LGLSXP:  break;
    case LISTSXP: {
      SEXP res_cpy, res_next, res_next_next, res_val;
      for(
        res_cpy = res; res_cpy != R_NilValue;
        res_cpy = res_next
      ) {
        res_next = CDR(res_cpy);
        res_val = CAR(res_cpy);
        if(TYPEOF(res_val) != STRSXP)
          error("Logic Error: non string value in return pairlist; contact maintainer.");
        if(res_next == R_NilValue) break;
        if(R_compute_identical(CAR(res_next), res_val, 16)) { // Need to remove res_next
          res_next_next = CDR(res_next);
          SETCDR(res_cpy, res_next_next);
      } }
    } break;
    default:
      error("Logic Error: unexpected evaluating return type; contact maintainer.");
  }
  UNPROTECT(2);  // This seems a bit stupid, PROTECT/UNPROTECT
  return(res);
}
