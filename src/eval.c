/*
Copyright (C) 2017  Brodie Gaslam

This file is part of "vetr - Trust, but Verify"

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
*/

#include "validate.h"

/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */
/*
 * @param arg_lang the substituted language corresponding to the argument
 * @param arg_tag the argument name
 */

SEXP VALC_evaluate_recurse(
  SEXP lang, SEXP act_codes, SEXP arg_value, SEXP arg_lang, SEXP arg_tag,
  SEXP lang_full, struct VALC_settings set
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
      // nocov start
      error("%s%s"
        "Internal Error: mismatched language and eval type tracking 1; contact ",
        "maintainer."
      );
      // nocov end
    }
    if(TYPEOF(CAR(act_codes)) != INTSXP) {
      // nocov start
      error("%s%s",
        "Internal error: no integer codes produced by parsing process, which ",
        "should not happen; contact maintainer."
      );
      // nocov end
    } else {
      mode=asInteger(CAR(act_codes));
    }
  } else {
    if(TYPEOF(lang) == LANGSXP || TYPEOF(lang) == LISTSXP) {
      // nocov start
      error("%s%s",
        "Internal Error: mismatched language and eval type tracking 2; contact ",
        "maintainer."
      );
      // nocov end
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
            CAR(lang), CAR(act_codes), arg_value, arg_lang, arg_tag, lang_full,
            set
        ) );
        if(TYPEOF(eval_res) == LISTSXP) {
          if(mode == 1) {
            // At least one non-TRUE result, which is a failure, so return
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
          // nocov start
          error("%s%s",
            "Internal Error: unexpected return value when recursively ",
            "evaluating parse; contact maintainer."
          );
          // nocov end
        }
        lang = CDR(lang);
        act_codes = CDR(act_codes);
        parse_count++;
        UNPROTECT(1);
      }
      if(parse_count != 2) {
        // nocov start
        error("%s%s",
          "Internal Error: unexpected language structure for modes 1/2; ",
          "contact maintainer."
        );
        // nocov end
      }
      UNPROTECT(1);
      // Only way to get here is if none of previous actually returned TRUE and
      // mode is OR
      if(mode == 2) {
        return(err_list);
      }
      return(eval_res);  // Or if all returned TRUE and mode is AND
    } else {
      // nocov start
      error(
        "%s%s",
        "Internal Error: in mode c(1, 2), but not a language object; ",
        "contact maintainer."
      );
      // nocov end
    }
  } else if(mode == 10 || mode == 999) {
    SEXP eval_res, eval_tmp;
    int err_val = 0;
    int eval_res_c = -1000;  // initialize to illegal value
    int * err_point = &err_val;
    eval_tmp = PROTECT(R_tryEval(lang, set.env, err_point));
    if(* err_point) {
      VALC_arg_error(
        arg_tag, lang_full,
        "Validation expression for argument `%s` produced an error (see previous error)."
      );
    }
    if(mode == 10) {
      eval_res_c = VALC_all(eval_tmp);
      eval_res = PROTECT(ScalarLogical(eval_res_c > 0));
    } else {
      eval_res = PROTECT(ALIKEC_alike_int2(eval_tmp, arg_value, arg_lang, set));
    }
    // Sanity checks

    int is_tf = TYPEOF(eval_res) == LGLSXP && XLENGTH(eval_res) == 1L &&
      asInteger(eval_res) != NA_INTEGER;
    int is_val_string = TYPEOF(eval_res) == STRSXP &&
      (XLENGTH(eval_res) == 5 || XLENGTH(eval_res) == 1);

    if(!(is_tf || is_val_string)) {
      // nocov start
      error("%s %s (is type: %s), %s",
        "Internal Error: token eval must be TRUE, FALSE, character(1L), ",
        " or character(5L)", type2char(TYPEOF(eval_res)), "contact maintainer."
      );
      // nocov end
    }
    // Note we're handling both user exp and template eval here

    if(
      TYPEOF(eval_res) != LGLSXP || !asLogical(eval_res)
    ) {
      SEXP err_msg = PROTECT(allocList(1));
      // mode == 10 is user eval, special treatment to produce err msg

      if(mode == 10) {
        // Tried to do this as part of init originally so we don't have to repeat
        // wholesale creation of call, but the call elements kept getting over
        // written by other stuff.  Not sure how to protect in calls defined at
        // top level

        SEXP err_attrib;
        const char * err_call;

        // If message attribute defined, this is easy:

        if((err_attrib = getAttrib(lang, VALC_SYM_errmsg)) != R_NilValue) {
          if(TYPEOF(err_attrib) != STRSXP || XLENGTH(err_attrib) != 1) {
            VALC_arg_error(
              arg_tag, lang_full,
              "\"err.msg\" attribute for validation token for argument `%s` must be a one length character vector."
            );
          }
          err_call = ALIKEC_pad_or_quote(arg_lang, set.width, -1, set);

          // Need to make copy of string, modify it, and turn it back into
          // string

          const char * err_attrib_msg = CHAR(STRING_ELT(err_attrib, 0));
          char * err_attrib_mod = CSR_smprintf4(
            set.nchar_max, err_attrib_msg, err_call, "", "", ""
          );
          // not protecting mkString since assigning to protected object
          SETCAR(err_msg, mkString(err_attrib_mod));
        } else {
          // message attribute not defined, must construct error message based
          // on result of evaluation

          err_call = ALIKEC_pad_or_quote(lang, set.width, -1, set);

          char * err_str;
          char * err_tok;
          switch(eval_res_c) {
            case -2: {
              const char * err_tok_tmp = type2char(TYPEOF(eval_tmp));
              const char * err_tok_base = "is \"%s\" instead of a \"logical\"";
              err_tok = R_alloc(
                strlen(err_tok_tmp) + strlen(err_tok_base), sizeof(char)
              );
              if(sprintf(err_tok, err_tok_base, err_tok_tmp) < 0)
                // nocov start
                error(
                  "Internal error: build token error failure; contact maintainer"
                );
                // nocov end
              }
              break;
            case -1: err_tok = "FALSE"; break;
            case -3: err_tok = "NA"; break;
            case -4: err_tok = "contains NAs"; break;
            case -5: err_tok = "zero length"; break;
            case 0: err_tok = "contains non-TRUE values"; break;
            default: {
              // nocov start
              error(
                "Internal Error: %s %d; contact maintainer.",
                "unexpected user exp eval value", eval_res_c
              );
              // nocov end
            }
          }
          const char * err_extra_a = "is not all TRUE";
          const char * err_extra_b = "is not TRUE"; // must be shorter than _a
          const char * err_extra;
          if(eval_res_c == 0) {
            err_extra = err_extra_a;
          } else {
            err_extra = err_extra_b;
          }
          const char * err_base = "%s%s (%s)";
          err_str = R_alloc(
            strlen(err_call) + strlen(err_base) + strlen(err_tok) +
            strlen(err_extra), sizeof(char)
          );
          // not sure why we're not using cstringr here
          if(sprintf(err_str, err_base, err_call, err_extra, err_tok) < 0) {
            // nocov start
            error(
              "%s%s", "Internal Error: could not construct error message; ",
              "contact maintainer."
            );
            // nocov end
          }
          SETCAR(err_msg, mkString(err_str));
        }
      } else { // must have been `alike` eval
        SETCAR(err_msg, eval_res);
      }
      UNPROTECT(3);
      return(err_msg);
    }
    UNPROTECT(2);
    return(eval_res);  // this should be `TRUE`
  } else {
    error("Internal Error: unexpected parse mode %d", mode);  // nocov
  }
  error("Internal Error: should never get here");             // nocov
  return(R_NilValue);  // nocov Should never get here
}
/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */
/*
TBD if this should call `VALC_parse` directly or not

@param lang the validator expression
@param arg_lang the substituted language being validated
@param arg_tag the variable name being validated
@param arg_value the value being validated
@param lang_full solely so that we can produce error message with original call
@param set the settings
*/
SEXP VALC_evaluate(
  SEXP lang, SEXP arg_lang, SEXP arg_tag, SEXP arg_value, SEXP lang_full,
  struct VALC_settings set
) {
  if(!IS_LANG(arg_lang))
    error("Internal Error: argument `arg_lang` must be language.");  // nocov

  SEXP lang_parsed = PROTECT(VALC_parse(lang, arg_lang, set));
  SEXP res = PROTECT(
    VALC_evaluate_recurse(
      VECTOR_ELT(lang_parsed, 0),
      VECTOR_ELT(lang_parsed, 1),
      arg_value, arg_lang, arg_tag, lang_full, set
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
        if(TYPEOF(res_val) != STRSXP) {
          // nocov start
          error(
            "%s%s",
            "Internal Error: non string value in return pairlist; contact ",
            "maintainer."
          );
          // nocov end
        }
        if(res_next == R_NilValue) break;
        // Need to remove res_next
        if(R_compute_identical(CAR(res_next), res_val, 16)) {
          res_next_next = CDR(res_next);
          SETCDR(res_cpy, res_next_next);
      } }
    } break;
    default: {
      // nocov start
      error(
        "Internal Error: unexpected evaluating return type; contact maintainer."
      );
      // nocov end
    }
  }
  UNPROTECT(2);  // This seems a bit stupid, PROTECT/UNPROTECT
  return(res);
}
SEXP VALC_evaluate_ext(
  SEXP lang, SEXP arg_lang, SEXP arg_tag, SEXP arg_value, SEXP lang_full,
  SEXP rho
) {
  struct VALC_settings set = VALC_settings_vet(R_NilValue, rho);
  return VALC_evaluate(lang, arg_lang, arg_tag, arg_value, lang_full, set);
}
