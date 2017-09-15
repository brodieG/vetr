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
/*
 * Result has a SEXP in .list_sxp that must be protected.
 */
struct VALC_res_list VALC_res_list_init(struct VALC_settings set) {
  if(set.result_list_size_init < 1)
    error("Internal Error: result alloc < 1; contact maintainer."); // nocov
  if(set.result_list_size_max < set.result_list_size_init)
    // nocov start
    error(
      "Internal Error: result max alloc less than alloc, contact maintainer"
    );
    // nocov end

  struct VALC_res_node * list_start = (struct VALC_res_node *) R_alloc(
    set.result_list_size_init, sizeof(struct VALC_res_node)
  );

  struct VALC_res_list res_list = (struct VALC_res_list) {
    .idx = 0,
    .idx_alloc = set.result_list_size_init,
    .idx_alloc_max = set.result_list_size_max,
    .list_tpl = list_start,
    .list_sxp = PROTECT(list1(R_NilValue))
  };
  res_list.list_sxp_tail = res_list.list_sxp;
  UNPROTECT(1);
  return res_list;
}
struct VALC_res_list VALC_res_add(
  struct VALC_res_list list, struct VALC_res res
) {
  if(list.idx > list.idx_alloc) {
    // nocov start
    error(
      "Internal Error: res list index greater than alloc, contact maintainer."
    );
    // nocov end
  } else if (list.idx == list.idx_alloc) {
    // Need to allocate more memory

    if(list.idx_alloc_max > list.idx_alloc) {
      int alloc_size;

      if(list.idx_alloc_max - list.idx_alloc < list.idx_alloc) {
        // No room to double, alloc to max

        alloc_size = list.idx_alloc_max;
      } else {
        alloc_size = list.idx_alloc * 2;
      }
      list.list_tpl = (struct VALC_res_node *) S_realloc(
        (char *) list.list_tpl, (long) alloc_size,
        (long) list.idx_alloc, sizeof(struct VALC_res_node)
      );
      list.idx_alloc = alloc_size;
    } else {
      error(
        "%s (%d); %s%s%s%s",
        "Reached maximum vet token result buffer size",
        list.idx_alloc_max,
        "this should only happen if you have more than that number of tokens ",
        "compounded with `||`.  If that is the case, see description of ",
        "`result.list.size` parameter for `?vetr_settings`.  If not, contact ",
        "maintainer."
      );
    }
  }
  list.list_tpl[list.idx] = (struct VALC_res_node) {
    .tpl_dat = res.dat.tpl_dat,
    .tpl = res.tpl,
    .success = res.success
  };
  ++list.idx;

  SETCAR(list.list_sxp_tail, res.dat.sxp_dat);
  SETCDR(list.list_sxp_tail, list1(R_NilValue));
  list.list_sxp_tail = CDR(list.list_sxp_tail);

  return(list);
}
/*
 * val_res should be a vector containing character vectors in each position
 * and each of those character vectors should be length one if produced by
 * standard tokens, or length 5 if they were produced by templates.
 *
 * @param val_res is the return value of VALC_evaluate
 * @param val_tag is the argument name in questions
 * @param fun_call (unsure?) is the original function call to use when throwing
 *   the error
 * @param ret_mode is the mode of the return value
 * - 0 = default, error message as length 1 character vector with everything
 *   except the introductory part of the error message
 * - 1 = entire error message returned as character(1L)
 * - 2 = pieces of error message returned as character(N)
 * @param stop if 1 then stop, otherwise return
 */

SEXP VALC_process_error(
  SEXP val_res, SEXP val_tag, SEXP fun_call, int ret_mode, int stop,
  struct VALC_settings set
) {
  // - Failure / Validation ----------------------------------------------------

  // Failure, explain why; two pass process because we first need to determine
  // size of error, allocate, then convert to char

  if(TYPEOF(val_res) != VECSXP)
    // nocov start
    error(
      "Internal Error: unexpected type %s when evaluating test for %s; %s",
      type2char(TYPEOF(val_res)), CHAR(PRINTNAME(val_tag)),
      "contact mainainer."
    );
    // nocov end
  if(ret_mode < 0 || ret_mode > 2)
    // nocov start
    error(
      "%s%s", "Internal Error: arg ret_mode must be between 0 and 2; ",
      "contact maintainer."
    );
    // nocov end

  if(!xlength(val_res)) return VALC_TRUE;

  // Compose optional argument part of message. This ends up being "Argument
  // `x` should %s" where arg and should are optional

  char * err_arg_msg = "";
  const char * err_arg = CHAR(PRINTNAME(val_tag));
  const char * err_very_base = "For argument `%s`%s%s%s";

  if(ret_mode == 1) {
    err_arg_msg = CSR_smprintf4(
      set.nchar_max, err_very_base, err_arg, "", "", ""
    );
  }
  const char * err_base = "%s%%s%%s%s%s%s";
  char * err_base_msg = CSR_smprintf4(
    set.nchar_max, err_base, err_arg_msg, "", "", ""
  );
  // Collapse similar entries into one; from this point on every entry in the
  // list should be a character(1L)

  SEXP err_msg_c = PROTECT(ALIKEC_merge_msg_2(val_res, set));
  R_xlen_t i, err_len = XLENGTH(err_msg_c);

  // Transfer to a character vector from list, also convert to bullets if
  // needed.  Not super efficient because we're writing back to an R vector
  // instead of just continuing all the processing in C strings, but made it a
  // little simpler to handle the rest of the code.

  int has_header = ret_mode != 2 && err_len > 1;
  SEXP err_vec_res = PROTECT(allocVector(STRSXP, err_len + has_header));

  if(has_header) {
    SET_STRING_ELT(err_vec_res, 0, mkChar(""));  // will add header later
  }
  for(i = 0; i < err_len; i++) {
    SEXP str = VECTOR_ELT(err_msg_c, i);
    if(TYPEOF(str) != STRSXP || XLENGTH(str) != 1L) {
      // nocov start
      error(
        "Internal Error: did not get character(1L) err msg; contact maintainer"
      );
      // nocov end
    }

    SEXP new_elt;
    SEXP old_elt = STRING_ELT(str, 0);

    if(err_len > 1 && ret_mode != 2) {
      new_elt = PROTECT(
        mkChar(CSR_bullet(CHAR(old_elt), "  - ", "    ", set.nchar_max))
      );
    } else {
      new_elt = PROTECT(old_elt);
    }
    SET_STRING_ELT(err_vec_res, i + has_header, new_elt);
    UNPROTECT(1);
  }
  if(!stop && ret_mode == 2) {
    // In this case we return the actual vector, in all others we need to
    // generate the string; this is handled by the !stop bit further down
  } else {
    if(err_len == 1) {
      // Here we need to compose the full character value since there is only
      // one correct value for the arg

      const char * err_msg_orig = CHAR(asChar(err_vec_res));
      char * err_msg = CSR_strmcpy(err_msg_orig, set.nchar_max);
      if(err_msg) err_msg[0] = tolower(err_msg[0]);

      const char * err_interim = "";
      if(ret_mode == 1) err_interim = ", ";

      char * err_full = CSR_smprintf4(
        set.nchar_max, err_base_msg, err_interim, err_msg, "", ""
      );
      SET_STRING_ELT(err_vec_res, 0, mkChar(err_full));
    } else if(has_header) {
      // Have multiple "or" cases

      const char * err_interim = "";
      if(ret_mode == 1) {
        err_interim = " at least one of these should pass:";
      } else if(!ret_mode) {
        err_interim = "At least one of these should pass:";
      }
      char * err_head = CSR_smprintf4(
        set.nchar_max, err_base_msg, err_interim, "", "", ""
      );
      SET_STRING_ELT(err_vec_res, 0, mkChar(err_head));
    }
  }
  UNPROTECT(2);  // unprotects vector result
  if(!stop) {
    return err_vec_res;
  } else {
    char * err_full = CSR_collapse(err_vec_res, "\n", set.nchar_max);
    VALC_stop(fun_call, err_full);
  }
  // nocov start
  error("%s",
    "Internal Error: this code should not evaluate; contact maintainer 2745."
  );
  // nocov end
}
/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */

SEXP VALC_validate(
  SEXP target, SEXP current, SEXP cur_sub, SEXP par_call, SEXP rho,
  SEXP ret_mode_sxp, SEXP stop, SEXP settings
) {
  SEXP res;
  if(TYPEOF(ret_mode_sxp) != STRSXP && XLENGTH(ret_mode_sxp) != 1)
    error("`vet` usage error: argument `format` must be character(1L).");
  int stop_int;

  if(
    (TYPEOF(stop) != LGLSXP && XLENGTH(stop) != 1) ||
    ((stop_int = asInteger(stop)) == NA_INTEGER)
  )
    error("`vet` usage error: argument `stop` must be TRUE or FALSE.");

  if(TYPEOF(rho) != ENVSXP)
    error(
      "`vet` usage error: argument `env` must be an environment (is %s).",
      type2char(TYPEOF(rho))
    );

  struct VALC_settings set = VALC_settings_vet(settings, rho);
  res = PROTECT(
    VALC_evaluate(
      target, cur_sub,
      TYPEOF(cur_sub) == SYMSXP ? cur_sub : VALC_SYM_current,
      current, par_call, set
    )
  );
  if(!xlength(res)) {
    UNPROTECT(1);
    return(ScalarLogical(1));
  }
  const char * ret_mode_chr = CHAR(asChar(ret_mode_sxp));
  int ret_mode;

  if(!strcmp(ret_mode_chr, "text")) {
    ret_mode = 0;
  } else if(!strcmp(ret_mode_chr, "raw")) {
    ret_mode = 2;
  } else if(!strcmp(ret_mode_chr, "full")) {
    ret_mode = 1;
  } else
    error(
      "%s%s",
      "`vet` usage error: argument `format` must be one of \"text\", \"raw\", ",
      "\"full\""
    );

  SEXP out = VALC_process_error(
    res, VALC_SYM_current, par_call, ret_mode, stop_int, set
  );
  UNPROTECT(1);
  return out;
}

/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */

SEXP VALC_validate_args(
  SEXP fun, SEXP fun_call, SEXP val_call, SEXP fun_frame, SEXP settings
) {
  // For now just use default settings

  struct VALC_settings set = VALC_settings_vet(settings, fun_frame);
  set.env = fun_frame;

  // For the elements with validation call setup, check for errors;  Note that
  // we need to skip the first element of the calls since we only care about the
  // args.

  SEXP val_call_cpy, fun_call_cpy, fun_form_cpy;
  // note `fun` will always be a closure
  SEXP fun_form = FORMALS(fun);
  // `fun_form` is only the formals so we don't need to skip the first value
  fun_form_cpy = fun_form;
  for(
    val_call_cpy = CDR(val_call),
    fun_call_cpy = CDR(fun_call);
    fun_form_cpy != R_NilValue;
    fun_form_cpy = CDR(fun_form_cpy),
    val_call_cpy = CDR(val_call_cpy),
    fun_call_cpy = CDR(fun_call_cpy)
  ) {
    SEXP arg_tag, val_tag, frm_tag;

    // It is possible for the function call to have more arguments than the
    // validation call, but for both the arguments should be in the same order

    val_tag = TAG(val_call_cpy);
    while(fun_form_cpy != R_NilValue) {
      frm_tag = TAG(fun_form_cpy);
      arg_tag = TAG(fun_call_cpy);
      if(val_tag != frm_tag) {
        fun_form_cpy = CDR(fun_form_cpy);
        if(frm_tag == arg_tag) fun_call_cpy = CDR(fun_call_cpy);
      } else {
        break;
      }
    }
    arg_tag = TAG(fun_call_cpy);
    frm_tag = TAG(fun_form_cpy);

    if(val_tag != frm_tag) {
      // nocov start
      error(
        "%s%s", "Internal Error: validation token does not match formals; ",
        "contact maintainer."
      );
      // nocov end
    }
    // Either our function is improperly missing an argument, or we have
    // validation for a default argument.  Note that since default arguments can
    // reference other arguments, we can't just assume that the default value is
    // completely reasonable, although.

    SEXP val_tok, fun_tok = R_MissingArg;
    if(arg_tag != frm_tag) {
      if(CAR(fun_form_cpy) != R_MissingArg) {
        arg_tag = frm_tag;
        fun_tok = CAR(fun_form_cpy);
      } else {
        VALC_arg_error(
          frm_tag, fun_call, "argument `%s` is missing, with no default"
        );
      }
    } else {
      fun_tok = CAR(fun_call_cpy);
    }
    val_tok = CAR(val_call_cpy);
    if(val_tok == R_MissingArg) {
      // nocov start
      error(
        "Internal Error: vetting expression unmatched; contact maintainer."
      );
      // nocov end
    }

    if(fun_tok == R_MissingArg) {
      // this shouldn't really happen now that we're using match.call instead of
      // match_call
      // nocov start
      error(
        "Internal Error: unexpected missing arg; contact maintainer."
      );
      VALC_arg_error(TAG(fun_call_cpy), fun_call, "Argument `%s` is missing");
      // nocov end
    }
    // Need to evaluate the argument

    int err_val = 0;
    int * err_point = &err_val;

    // Force evaluation of argument in fun frame, which should cause the
    // corresponding promise to be evaluated in the correct frame

    SEXP fun_val = R_tryEval(arg_tag, fun_frame, err_point);
    if(* err_point) {
      VALC_arg_error(
        arg_tag, fun_call,
        "Argument `%s` produced error during evaluation; see previous error."
    );}
    // Evaluate the validation expression

    SEXP val_res = PROTECT(
      VALC_evaluate(val_tok, fun_tok, arg_tag, fun_val, val_call, set)
    );
    if(xlength(val_res)) {
      // fail, produce error message: NOTE - might change if we try to use full
      // expression instead of just arg name
      VALC_process_error(val_res, arg_tag, fun_call, 1, 1, set);
      // nocov start
      error("Internal Error: should never get here 2487; contact maintainer");
      // nocov end
    }
    UNPROTECT(1);
  }
  if(val_call_cpy != R_NilValue || fun_call_cpy != R_NilValue) {
    // nocov start
    error(
      "%s%s", "Internal Error: fun and validation matched calls different ",
      "lengths; contact maintainer."
    );
    // nocov end
  }
  return VALC_TRUE;
}
