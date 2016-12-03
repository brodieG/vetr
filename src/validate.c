#include "validate.h"
/*
 * val_res should be a pairlist containing character vectors in each position
 * and each of those character vectors should be length one
 *
 * ret_mode is the mode of the return value
 * - 0 = default, error message as length 1 character vector with everything
 *   except the "Argument ..." part
 * - 1 = entire error message returned as character(1L)
 * - 2 = pieces of error message returned as character(N)
 * - 3 = error
 */

SEXP VALC_process_error(
  SEXP val_res, SEXP val_tag, SEXP fun_call, int ret_mode
) {
  // - Failure / Validation ----------------------------------------------------

  // Failure, explain why; two pass process because we first need to determine
  // size of error, allocate, then convert to char

  if(TYPEOF(val_res) != LISTSXP)
    error(
      "Logic Error: unexpected type %s when evaluating test for %s; contact mainainer.",
      type2char(TYPEOF(val_res)), CHAR(PRINTNAME(val_tag))
    );
  if(ret_mode < 0 || ret_mode > 3)
    error("Logic Error: arg ret_mode must be between 0 and 4; contact maintainer.");

  SEXP val_res_cpy;
  size_t count_top = 0, size = 0;

  // Compose optional argument part of message. This ends up being "Argument
  // `x` should %s" where arg and should are optional

  char * err_arg_msg = "";
  if(ret_mode == 1 || ret_mode == 3) err_arg_msg = "Argument ";

  const char * err_base = "%s%s%%s";
  char * err_base_msg = CSR_smprintf4(
    VALC_MAX_CHAR, err_base, err_arg_msg, "", "", ""
  );

  // First pass get sizes; note that prior to commit e3724f9 char vectors with
  // length greater than one were allowed, but that outcome should not occur
  // so we no longer support it; look at prior commits for the code to support
  // that

  for(
    val_res_cpy = val_res; val_res_cpy != R_NilValue;
    val_res_cpy = CDR(val_res_cpy)
  ) {
    SEXP err_vec = CAR(val_res_cpy);
    if(TYPEOF(err_vec) != STRSXP)
      error("Logic Error: did not get character err msg; contact maintainer");
    if(XLENGTH(err_vec) != 1)
      error("Logic Error: no longer support err msgs greater than length one; contact maintainer");

    count_top++;
    size += CSR_strmlen(CHAR(STRING_ELT(err_vec, 0)), VALC_MAX_CHAR);
  }
  if(!count_top) return VALC_TRUE;

  // Depending on whether there is one error or multiple ones (multiple means
  // value failed to match any of the OR possibilities), render error; we render
  // directly into a STRSXP because we might want to return that and it is
  // simpler to do that even if it turns out in the end we just want to
  // return the full error.

  char * err_full;
  SEXP err_vec_res = PROTECT(
    allocVector(STRSXP, ret_mode == 2 ? count_top : 1)
  );
  // Handle simplest case where we just return the unmodified error messages
  // by copying to a new string

  if(ret_mode == 2) {
    size_t count = 0;
    for(
      val_res_cpy = val_res; val_res_cpy != R_NilValue;
      val_res_cpy = CDR(val_res_cpy)
    ) {
      // mkChar/CHAR business may not be necessary; also not protecting since
      // we're assigning to a protected object (this is a good place to look
      // if we start getting C crashes...)

      SET_STRING_ELT(
        err_vec_res, count, mkChar(CHAR(STRING_ELT(CAR(val_res_cpy), 0)))
      );
      count++;
    }
  } else {
    // Here we need to compose the full character value
    if(count_top == 1) {
      char * err_msg = CSR_strmcpy(CHAR(asChar(CAR(val_res))), VALC_MAX_CHAR);
      if(err_msg) err_msg[0] = tolower(err_msg[0]);

      err_full = CSR_smprintf4(
        VALC_MAX_CHAR, err_base_msg, err_msg, "", "", ""
      );
      SET_STRING_ELT(err_vec_res, 0, mkChar(err_full));
    } else {
      char * err_head = CSR_smprintf4(
        VALC_MAX_CHAR, err_base_msg, "meet at least one of the following:\n",
        "", "", ""
      );
      size += CSR_strmlen(err_head, VALC_MAX_CHAR) + 5 * count_top;

      // Need to use base C string manip because we are writing each line
      // sequentially to the pre-allocated block

      err_full = R_alloc(size + 1, sizeof(char));
      char * err_full_cpy = err_full;

      sprintf(err_full_cpy, "%s", err_head);
      err_full_cpy = err_full_cpy + CSR_strmlen(err_full_cpy, VALC_MAX_CHAR);

      // Second pass construct string

      size_t count = 0;
      for(
        val_res_cpy = val_res; val_res_cpy != R_NilValue;
        val_res_cpy = CDR(val_res_cpy)
      ) {
        SEXP err_vec = CAR(val_res_cpy);

        char * err_sub = CSR_strmcpy(
          CHAR(STRING_ELT(err_vec, 0)), VALC_MAX_CHAR
        );
        sprintf(err_full_cpy, "  - %s\n", err_sub);
        err_full_cpy += strlen(err_sub) + 5;
        count++;
      }
    }
    SET_STRING_ELT(err_vec_res, 0, mkChar(err_full));
  }
  // Return TRUE, message, or throw an error depending on user input

  UNPROTECT(1);  // unprotects vector result
  if(ret_mode != 3) {
    return err_vec_res;
  } else {
    VALC_stop(fun_call, err_full);
    error("Logic Error: this code should not evaluate; contact maintainer 2745.");
  }
}
/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */

SEXP VALC_validate(
  SEXP target, SEXP current, SEXP par_call, SEXP rho, SEXP ret_mode_sxp
) {
  SEXP res;
  res = PROTECT(
    VALC_evaluate(
      target, VALC_SYM_current, VALC_SYM_current, current, par_call, rho
  ) );
  if(IS_TRUE(res)) {
    UNPROTECT(1);
    return(VALC_TRUE);
  }
  if(TYPEOF(ret_mode_sxp) != STRSXP && XLENGTH(ret_mode_sxp) != 1)
    error("Argument `return.mode` must be character(1L)");
  const char * ret_mode_chr = CHAR(asChar(ret_mode_sxp));
  int ret_mode;

  if(!strcmp(ret_mode_chr, "text")) {
    ret_mode = 0;
  } else if(!strcmp(ret_mode_chr, "stop")) {
    ret_mode = 3;
  } else if(!strcmp(ret_mode_chr, "raw")) {
    ret_mode = 2;
  } else if(!strcmp(ret_mode_chr, "full")) {
    ret_mode = 1;
  } else error("Argument `return.mode` must be one of \"text\", \"stop\", \"raw\",\"full\"");
  SEXP out = VALC_process_error(res, VALC_SYM_current, par_call, ret_mode);
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
    SEXP arg_tag;

    if(TAG(val_call_cpy) != (arg_tag = TAG(fun_call_cpy)))
      error("Logic Error: tag mismatch between function and validation; contact maintainer.");

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
    VALC_process_error(val_res, TAG(fun_call_cpy), fun_call, 3);
    error("Logic Error: should never get here 2487; contact maintainer");
  }
  if(val_call_cpy != R_NilValue || fun_call_cpy != R_NilValue)
    error("Logic Error: fun and validation matched calls different lengths; contact maintainer.");

  // Match the calls up
  UNPROTECT(4);
  UNPROTECT(4); // SEXPs used as arguments for match_call
  return VALC_TRUE;
}
