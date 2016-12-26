#include "validate.h"
/*
 * val_res should be a pairlist containing character vectors in each position
 * and each of those character vectors should be length one
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
  SEXP val_res, SEXP val_tag, SEXP fun_call, int ret_mode, int stop
) {
  // - Failure / Validation ----------------------------------------------------

  // Failure, explain why; two pass process because we first need to determine
  // size of error, allocate, then convert to char

  PrintValue(val_res);
  PrintValue(val_tag);
  PrintValue(fun_call);
  Rprintf("hello %d\n", 23412);
  if(TYPEOF(val_res) != LISTSXP)
    error(
      "Internal Error: unexpected type %s when evaluating test for %s; %s",
      type2char(TYPEOF(val_res)), CHAR(PRINTNAME(val_tag)),
      "contact mainainer."
    );
  if(ret_mode < 0 || ret_mode > 2)
    error(
      "%s%s", "Internal Error: arg ret_mode must be between 0 and 2; ",
      "contact maintainer."
    );

  SEXP val_res_cpy;
  size_t count_top = 0;
  R_xlen_t count_lines = 0;

  // Compose optional argument part of message. This ends up being "Argument
  // `x` should %s" where arg and should are optional

  char * err_arg_msg = "";
  const char * err_arg = CHAR(PRINTNAME(val_tag));
  const char * err_very_base = "For argument `%s`";

  if(ret_mode == 1) {
    err_arg_msg = CSR_smprintf4(
      VALC_MAX_CHAR, err_very_base, err_arg, "", "", ""
    );
  }
  const char * err_base = "%s%%s%%s";
  char * err_base_msg = CSR_smprintf4(
    VALC_MAX_CHAR, err_base, err_arg_msg, "", "", ""
  );
  // First count how many items we have as we need different treatment depending
  // on what we're looking at

  for(
    val_res_cpy = val_res; val_res_cpy != R_NilValue;
    val_res_cpy = CDR(val_res_cpy)
  ) {
    count_top++;
    count_lines += XLENGTH(CAR(val_res_cpy));
  }
  if(!count_top || !count_lines) return VALC_TRUE;

  // Transfer to a character vector from list, also convert to bullets if
  // needed.  Not super efficient because we're writing back to an R vector
  // instead of just continuing all the processing in C strings, but made it a
  // little simpler to handle the rest of the code.

  int has_header = ret_mode != 2 && count_top > 1;
  SEXP err_vec_res = PROTECT(allocVector(STRSXP, count_lines + has_header));
  R_xlen_t str_count = 0;

  if(has_header) {
    SET_STRING_ELT(err_vec_res, 0, mkChar(""));  // will add header later
    str_count++;
  }
  for(
    val_res_cpy = val_res; val_res_cpy != R_NilValue;
    val_res_cpy = CDR(val_res_cpy)
  ) {
    SEXP str = CAR(val_res_cpy);
    if(TYPEOF(str) != STRSXP)
      error("Internal Error: did not get chr err msg; contact maintainer");

    for(R_xlen_t str_i = 0; str_i < XLENGTH(str); str_i++, str_count++) {
      SEXP new_elt;
      SEXP old_elt = STRING_ELT(str, str_i);

      if(count_top > 1) {
        new_elt = PROTECT(
          mkChar(VALC_bullet(CHAR(old_elt), "  - ", "    ", VALC_MAX_CHAR))
        );
      } else {
        new_elt = PROTECT(old_elt);
      }
      SET_STRING_ELT(err_vec_res, str_count, new_elt);
      UNPROTECT(1);
    }
  }
  if(!stop && ret_mode == 2) {
    // In this case we return the actual vector, in all others we need to
    // generate the string

    UNPROTECT(1);
    return(err_vec_res);
  } else {
    if(count_top == 1) {
      // Here we need to compose the full character value since there is only
      // one correct value for the arg

      char * err_msg =
        CSR_strmcpy(CHAR(asChar(CAR(err_vec_res))), VALC_MAX_CHAR);
      if(err_msg) err_msg[0] = tolower(err_msg[0]);

      const char * err_interim = "";
      if(ret_mode == 1) err_interim = ", ";

      char * err_full = CSR_smprintf4(
        VALC_MAX_CHAR, err_base_msg, err_interim, err_msg, "", ""
      );
      SET_STRING_ELT(err_vec_res, 0, mkChar(err_full));
    } else {
      // Have multiple "or" cases

      const char * err_interim = "";
      if(ret_mode == 1) {
        err_interim = " at least one of these should pass:\n";
      } else if(!ret_mode) {
        err_interim = "At least one of these should pass:\n";
      }
      char * err_head = CSR_smprintf4(
        VALC_MAX_CHAR, err_base_msg, err_interim, "", "", ""
      );
      SET_STRING_ELT(err_vec_res, 0, mkChar(err_head));
    }
    UNPROTECT(1);  // unprotects vector result
    if(!stop) {
      return err_vec_res;
    } else {
      char * err_full = CSR_collapse(err_vec_res, "\n", VALC_MAX_CHAR);
      VALC_stop(fun_call, err_full);
    }
  }
  error("%s",
    "Internal Error: this code should not evaluate; contact maintainer 2745."
  );
}
/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */

SEXP VALC_validate(
  SEXP target, SEXP current, SEXP cur_sub, SEXP par_call, SEXP rho,
  SEXP ret_mode_sxp, SEXP stop
) {
  SEXP res;
  res = PROTECT(
    VALC_evaluate(target, cur_sub, VALC_SYM_current, current, par_call, rho)
  );
  if(IS_TRUE(res)) {
    UNPROTECT(1);
    return(VALC_TRUE);
  }
  if(TYPEOF(ret_mode_sxp) != STRSXP && XLENGTH(ret_mode_sxp) != 1)
    error("Argument `return.mode` must be character(1L)");
  int stop_int;

  if(
    (TYPEOF(stop) != LGLSXP && XLENGTH(stop) != 1) ||
    ((stop_int = asInteger(stop)) == NA_INTEGER)
  )
    error("Argument `stop` must be TRUE or FALSE");

  const char * ret_mode_chr = CHAR(asChar(ret_mode_sxp));
  int ret_mode;

  if(!strcmp(ret_mode_chr, "text")) {
    ret_mode = 0;
  } else if(!strcmp(ret_mode_chr, "raw")) {
    ret_mode = 2;
  } else if(!strcmp(ret_mode_chr, "full")) {
    ret_mode = 1;
  } else error("Argument `return.mode` must be one of \"text\", \"raw\",\"full\"");

  SEXP out = VALC_process_error(
    res, VALC_SYM_current, par_call, ret_mode, stop_int
  );
  UNPROTECT(1);
  return out;
}

/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */

SEXP VALC_validate_args(SEXP sys_frames, SEXP sys_calls, SEXP sys_pars) {
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
  // Get definition of fun in original call; this unfortunately requires
  // repeating some of the logic in the step above, but is pretty fast

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
    SEXP arg_tag;

    if(TAG(val_call_cpy) != (arg_tag = TAG(fun_call_cpy)))
      error("Internal Error: tag mismatch between function and validation; contact maintainer.");

    SEXP val_tok, fun_tok;
    val_tok = CAR(val_call_cpy);
    if(val_tok == R_MissingArg) continue;
    fun_tok = CAR(fun_call_cpy);
    if(fun_tok == R_MissingArg)
      VALC_arg_error(TAG(fun_call_cpy), fun_call, "Argument `%s` is missing");
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

    SEXP val_res = VALC_evaluate(
      val_tok, CAR(fun_call_cpy), arg_tag, fun_val, val_call, fun_frame
    );
    if(IS_TRUE(val_res)) continue;  // success, check next
    // fail, produce error message: NOTE - might change if we try to use full
    // expression instead of just arg name
    VALC_process_error(val_res, TAG(fun_call_cpy), fun_call, 1, 1);
    error("Internal Error: should never get here 2487; contact maintainer");
  }
  if(val_call_cpy != R_NilValue || fun_call_cpy != R_NilValue)
    error("Internal Error: fun and validation matched calls different lengths; contact maintainer.");

  // Match the calls up
  UNPROTECT(4);
  UNPROTECT(4); // SEXPs used as arguments for match_call
  return VALC_TRUE;
}
