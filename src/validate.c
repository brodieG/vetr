#include "validate.h"
/*
 * val_res should be a pairlist containing character vectors in each position
 * and each of those character vectors should be length one
 */

SEXP VALC_process_error(
  SEXP val_res, SEXP val_tag, SEXP fun_call, int error
) {
  // - Failure ---------------------------------------------------------------

  // Failure, explain why; two pass process because we first need to determine
  // size of error, allocate, then convert to char

  if(TYPEOF(val_res) != LISTSXP)
    error(
      "Logic Error: unexpected type %s when evaluating test for %s; contact mainainer.",
      type2char(TYPEOF(val_res)), CHAR(PRINTNAME(val_tag))
    );
  if(error != 0 && error !=1)
    error("Logic Error: arg error must be 0 or 1");

  SEXP val_res_cpy;
  size_t count_top = 0, count_sub = 0, size = 0;

  // First pass get sizes; note that prior to commit e3724f9 char vectors with
  // length greater than one were allowed, but that outcome should not occur
  // so we no longer support it; look at prior commits for the code to support
  // that

  for(
    val_res_cpy = val_res; val_res_cpy != R_NilValue;
    val_res_cpy = CDR(val_res_cpy)
  ) {
    R_xlen_t err_items = xlength(err_vec), i;
    SEXP err_vec = CAR(val_res_cpy);
    if(TYPEOF(err_vec) != STRSXP)
      error("Logic Error: did not get character err msg; contact maintainer");
    if(XLENGTH(err_vec) != 1)
      error("Logic Error: no longer support err msgs greater than length one; contact maintainer")

    count_top++;
    for(i = 0; i < err_items; i++) {
      size += CSR_strmlen(CHAR(STRING_ELT(err_vec, i)), VALC_MAX_CHAR);
    }
  }
  // Depending on whether there is one error or multiple ones (multiple means
  // value failed to match any of the OR possibilities), render error

  const char * err_arg = CHAR(PRINTNAME(val_tag));

  if(count_top == 1) {

    const char * err_base = "Argument `%s` should %s";

    char * err_msg = CSR_strmcpy(CHAR(asChar(CAR(val_res))), VALC_MAX_CHAR);
    if(err_msg) err_msg[0] = tolower(err_msg[0]);

    char * err_full = CSR_smprintf4(
      VALC_MAX_CHAR, err_base, err_arg, err_msg, "", ""
    );
    VALC_stop(fun_call, err_full);
    error("Logic Error: should not get here, R error should have been thrown; contact maintainer.");
  } else if (count_top > 1) {
    const char * err_base = "Argument `%s` should meet at least one of the following:\n";

    size += strlen(err_base) + strlen(err_arg) + 5 * count_top + 12 * count_sub;

    char * err_final = R_alloc(size + 1, sizeof(char));
    char * err_final_cpy = err_final;

    sprintf(err_final_cpy, err_base, err_arg);
    err_final_cpy = err_final_cpy + strlen(err_final_cpy);

    // Second pass construct string

    size_t count = 0;
    for(
      val_res_cpy = val_res; val_res_cpy != R_NilValue;
      val_res_cpy = CDR(val_res_cpy)
    ) {
      SEXP err_vec = CAR(val_res_cpy);
      const R_xlen_t err_items = xlength(err_vec);
      R_xlen_t i;

      char * err_sub = CSR_strmcpy(CHAR(STRING_ELT(err_vec, 0)), VALC_MAX_CHAR);
      sprintf(err_final_cpy, "  - %s\n", err_sub);
      err_final_cpy += strlen(err_sub) + 5;
      count++;
    }
    VALC_stop(fun_call, err_final);
    error("Logic Error: should never get here 629; contact maintainer.");
  } else {
    error("Logic Error: no values in result error; contact maintainer.");
  }
  return VALC_TRUE;
}
/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */

SEXP VALC_validate(SEXP target, SEXP current, SEXP par_call, SEXP rho) {
  SEXP res;
  res = PROTECT(VALC_evaluate(target, VALC_SYM_current, current, par_call, rho));
  if(IS_TRUE(res)) {
    UNPROTECT(1);
    return(VALC_TRUE);
  }
  VALC_process_error(res, VALC_SYM_current, par_call);
  UNPROTECT(1);
  error("Logic Error: should never get here 124");
  return R_NilValue;
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

    SEXP val_res = VALC_evaluate(val_tok, arg_tag, fun_val, val_call, fun_frame);
    if(IS_TRUE(val_res)) continue;  // success, check next
    VALC_process_error(val_res, TAG(fun_call_cpy), fun_call);    // fail, produce error message
  }
  if(val_call_cpy != R_NilValue || fun_call_cpy != R_NilValue)
    error("Logic Error: fun and validation matched calls different lengths; contact maintainer.");

  // Match the calls up
  UNPROTECT(4);
  UNPROTECT(4); // SEXPs used as arguments for match_call
  return VALC_TRUE;
}
