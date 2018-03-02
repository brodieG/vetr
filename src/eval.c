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
 * See `VALC_evaluate` for param descriptions.
 *
 * There is some really tricky busines swith the protection stack here. For each
 * VALC_res struct we create, we get a SEXP, and we want to keep those around
 * until we process the error in `VALC_evaluate`, so this function introduces a
 * stack balance issue that `VALC_evaluate` needs to rectify with the
 * information embedded in `res_list`.
 *
 * ^^ update, this might be dated from back when we allowed function calls that
 * produced unbalanced PROTECT stacks
 *
  * See VALC_parse_recurse for details about distinction between lang/lang2
 */

struct VALC_res_list VALC_evaluate_recurse(
  SEXP lang, SEXP act_codes, SEXP lang2, SEXP arg_value, SEXP arg_lang,
  SEXP arg_tag, SEXP lang_full, struct VALC_settings set,
  struct VALC_res_list res_list
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
    if(
      (TYPEOF(lang) != LANGSXP && TYPEOF(lang) != LISTSXP) ||
      (TYPEOF(lang2) != LANGSXP && TYPEOF(lang2) != LISTSXP)
    ) {
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
    if(TYPEOF(lang) == LANGSXP || TYPEOF(lang2) == LISTSXP) {
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
      lang = CDR(lang);
      lang2 = CDR(lang2);
      act_codes = CDR(act_codes);

      while(lang != R_NilValue) {
        res_list = VALC_evaluate_recurse(
          CAR(lang), CAR(act_codes), CAR(lang2), arg_value, arg_lang, arg_tag,
          lang_full, set, res_list
        );
        // recall res_list.idx points to next available slot, not last result
        struct VALC_res_node res_val = res_list.list_tpl[res_list.idx - 1];

        if(!res_val.success && mode == 1) {
          return(res_list);
        } else if (res_val.success && mode == 2) {
          // At least one succes in OR mode
          return(res_list);
        }
        lang = CDR(lang);
        lang2 = CDR(lang2);
        act_codes = CDR(act_codes);
        parse_count++;
      }
      if(parse_count != 2) {
        // nocov start
        error("%s%s",
          "Internal Error: unexpected language structure for modes 1/2; ",
          "contact maintainer."
        );
        // nocov end
      }
      // Only way to get here is if none of previous actually returned TRUE and
      // mode is OR

      return(res_list);
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
    struct VALC_res eval_res;
    // Depending on whether we're dealing with a template or a standard token,
    // we'll need to mess with what value gets protected.  The input `res_list`
    // contains a PROTECTed LSTSXP that we'll use to protect everything

    PROTECT_INDEX ipx;
    SEXP eval_tmp, eval_dat;
    PROTECT_WITH_INDEX(eval_dat=allocVector(VECSXP, 2), &ipx);

    int err_val = 0;
    int eval_res_c = -1000;  // initialize to illegal value
    int * err_point = &err_val;
    eval_tmp = PROTECT(R_tryEval(lang, set.env, err_point));

    SET_VECTOR_ELT(eval_dat, 0, lang2);
    SET_VECTOR_ELT(eval_dat, 1, eval_tmp);
    UNPROTECT(1);

    if(* err_point) {
      VALC_arg_error(
        arg_tag, lang_full,
        "Validation expression for argument `%s` produced an error (see previous error)."
      );
    }
    if(mode == 10) {
      eval_res_c = VALC_all(VECTOR_ELT(eval_dat, 1));
      eval_res.tpl = 0;
      eval_res.success = eval_res_c > 0;
      eval_res.dat.sxp_dat = eval_dat;
    } else {
      eval_res.tpl = 1;
      // bit of a complicated protection mess here, we don't want eval_dat in
      // the protection stack when we're done, but we want the wrap in it, so we
      // use REPROTECT to take over its spot in the stack

      struct ALIKEC_res res_alike = ALIKEC_alike_internal(
        VECTOR_ELT(eval_dat, 1), arg_value, set
      );
      REPROTECT(res_alike.wrap, ipx);

      eval_res.dat.tpl_dat = res_alike.dat;
      eval_res.dat.sxp_dat = res_alike.wrap;

      eval_res.success = res_alike.success;
    }
    res_list = VALC_res_add(res_list, eval_res);
    UNPROTECT(1);
    return(res_list);
  } else {
    error("Internal Error: unexpected parse mode %d", mode);  // nocov
  }
  error("Internal Error: should never get here");             // nocov
  return(res_list);  // nocov Should never get here
}
/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */
/*
 * Helper funs to extract errors from eval recurse
 *
 * These produce a STRSXP containing the error details that can be assembled
 * into a final error message.  See `VALC_evaluate` for details on parameters.
 */
static SEXP VALC_error_standard(
  SEXP sxp_dat, SEXP arg_tag, SEXP arg_lang, SEXP sys_call,
  struct VALC_settings set
) {
  SEXP lang = VECTOR_ELT(sxp_dat, 0);
  SEXP eval_tmp = VECTOR_ELT(sxp_dat, 1);
  SEXP err_attrib;
  struct ALIKEC_pad_quote_res err_call;
  char * err_str;

  // If message attribute defined, this is easy:

  err_attrib = PROTECT(getAttrib(lang, VALC_SYM_errmsg));
  if(err_attrib != R_NilValue) {
    if(TYPEOF(err_attrib) != STRSXP || XLENGTH(err_attrib) != 1) {
      VALC_arg_error(
        arg_tag, sys_call,
        "\"err.msg\" attribute for validation token for argument `%s` must be a one length character vector."
      );
    }
    err_call = ALIKEC_pad_or_quote(arg_lang, set.width, -1, set);

    // Need to make copy of string, modify it, and turn it back into
    // string

    const char * err_attrib_msg = CHAR(STRING_ELT(err_attrib, 0));
    err_str = CSR_smprintf4(
      set.nchar_max, err_attrib_msg, err_call.chr, "", "", ""
    );
  } else {
    // message attribute not defined, must construct error message based
    // on result of evaluation

    err_call = ALIKEC_pad_or_quote(lang, set.width, -1, set);
    int eval_res_c = VALC_all(eval_tmp);

    char * err_tok;
    switch(eval_res_c) {
      case -6: {
          R_xlen_t eval_res_len = xlength(eval_tmp);
          // This could be slow, ideally we would avoid running CSR_smprintf
          err_tok = CSR_smprintf4(
            set.nchar_max,
            "is chr%s: \"%s\"%s%s",
            eval_res_len > 1 ?
              CSR_smprintf2(
                set.nchar_max, " [1:%s]%s", CSR_len_as_chr(eval_res_len),
                ""
              ) : "",
            CHAR(STRING_ELT(eval_tmp, 0)),
            eval_res_len > 1 ? " ..." : "",
            ""
          );
        }
        break;
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
      // case -5: err_tok = "zero length"; break;
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
    const char * err_base = "%s%s%s (%s)";

    int alloc_size = 0;
    int str_sizes[4] = {0, 0, 0, 0};

    str_sizes[0] = strlen(err_call.chr);
    str_sizes[1] = strlen(err_base);
    str_sizes[2] = strlen(err_extra);
    str_sizes[3] = strlen(err_tok);

    const char * extra_blank = "";
    if(!err_call.multi_line) extra_blank = " ";

    for(int i = 0; i < 4; ++i) {
      if(INT_MAX - str_sizes[i] < alloc_size)
        // nocov start
        error(
          "%s%s (%d)",
          "Internal Error: error string longer than INT_MAX; ",
          "contact maintainer.", i
        );
        // nocov end

      alloc_size += str_sizes[i];
    }
    err_str = R_alloc(alloc_size, sizeof(char));

    // not sure why we're not using cstringr here
    if(
      sprintf(
        err_str, err_base, err_call.chr, extra_blank, err_extra, err_tok
      ) < 0
    ) {
      // nocov start
      error(
        "%s%s", "Internal Error: could not construct error message; ",
        "contact maintainer."
      );
      // nocov end
    }
  }
  UNPROTECT(1);
  return mkString(err_str);
}
static SEXP VALC_error_template(
  struct ALIKEC_res_dat res, SEXP sxp_dat, SEXP arg_lang,
  struct VALC_settings set
) {
  struct ALIKEC_res res_alike = (struct ALIKEC_res) {
    .dat=res,
    .wrap=sxp_dat,
    .success=0  // we're assuming this
  };
  SEXP res_sxp = PROTECT(ALIKEC_res_as_strsxp(res_alike, arg_lang, set));
  if(TYPEOF(res_sxp) != STRSXP)
    // nocov start
    error(
      "Internal Error: %s %s , contact maintainer.",
      "result of processing template error must be STRSXP, is",
      type2char(TYPEOF(res_sxp))
    );
    // nocov end
  UNPROTECT(1);
  return res_sxp;
}
static SEXP VALC_error_extract(
  struct VALC_res_node res, SEXP sxp_dat, SEXP arg_tag, SEXP arg_lang,
  SEXP sys_call, struct VALC_settings set
) {
  if(res.tpl) {
    return VALC_error_template(res.tpl_dat, sxp_dat, arg_lang, set);
  } else {
    return VALC_error_standard(sxp_dat, arg_tag, arg_lang, sys_call, set);
  }
}
/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */
/*
@param lang the validator expression
@param arg_lang the substituted language being validated
@param arg_tag the variable name being validated
@param arg_value the value being validated
@param lang_full solely so that we can produce error message with original call
@param set the settings
@param use_lang_raw whether to use the raw language in evaluations, should be
  TRUE for `vet`/`tev`, but FALSE for `vetr` as for the latter we have to
  evaluate the version of the vetting token inside the function `vetr` is called
  in
*/
SEXP VALC_evaluate(
  SEXP lang, SEXP arg_lang, SEXP arg_tag, SEXP arg_value, SEXP lang_full,
  struct VALC_settings set, int use_lang_raw
) {
  if(!IS_LANG(arg_lang))
    error("Internal Error: argument `arg_lang` must be language.");  // nocov

  SEXP lang_parsed = PROTECT(VALC_parse(lang, arg_lang, set, arg_tag));
  struct VALC_res_list res_list, res_init = VALC_res_list_init(set);
  PROTECT(res_init.list_sxp);

  // Super wasteful, but if we are in vet/tev mode we don't actually need the
  // substituted language to evaluate and to show in error messages

  SEXP lang_eval =
    use_lang_raw ? VECTOR_ELT(lang_parsed, 2) : VECTOR_ELT(lang_parsed, 0);
  SEXP lang_msg = VECTOR_ELT(lang_parsed, 2);

  res_list = VALC_evaluate_recurse(
    lang_eval, VECTOR_ELT(lang_parsed, 1), lang_msg,
    arg_value, arg_lang, arg_tag, lang_full, set, res_init
  );
  if(res_list.idx == INT_MAX)
    // nocov start
    error("Internal Error: cannot have INT_MAX results, contact maintainer.");
    // nocov end

  // Now determine if we passed or failed, if idx is not zero means we had at
  // least one error.  There should be one error in AND mode, and possibly many
  // in OR mode.  Different rendering logic for template vs standard tokens.  In
  // all cases if the last recorded item is a success or there are no recorded
  // items, then we pass.

  SEXP res_as_str;

  if(!res_list.idx || res_list.list_tpl[res_list.idx - 1].success) {
    res_as_str = PROTECT(allocVector(VECSXP, 0));
  } else {
    // compute how many failures

    int fails = 0, j = 0;
    for(int i = 0; i < res_list.idx; ++i)
      fails += !res_list.list_tpl[i].success;
    res_as_str = PROTECT(allocVector(VECSXP, fails));
    SEXP sxp_dat = res_list.list_sxp;

    for(int i = 0; i < res_list.idx; ++i) {
      struct VALC_res_node res = res_list.list_tpl[i];
      if(sxp_dat == R_NilValue)
        // nocov start
        error(
          "Internal Error: SEXP and data list unsynchronized; %s",
          "contact maintainer"
        );
        // nocov end
      if(!res.success) {
        SET_VECTOR_ELT(
          res_as_str, j++,
          VALC_error_extract(
            res, CAR(sxp_dat), arg_tag, arg_lang, lang_full, set
          )
        );
      }
      sxp_dat = CDR(sxp_dat);
  } }
  // We used to remove duplicates here, but might make more sense to do so once
  // we get to the actual strings we're going to use so that we can sort and
  // check for repeated values.

  UNPROTECT(3);
  return(res_as_str);
}
SEXP VALC_evaluate_ext(
  SEXP lang, SEXP arg_lang, SEXP arg_tag, SEXP arg_value, SEXP lang_full,
  SEXP rho
) {
  struct VALC_settings set = VALC_settings_vet(R_NilValue, rho);
  return VALC_evaluate(lang, arg_lang, arg_tag, arg_value, lang_full, set, 0);
}
