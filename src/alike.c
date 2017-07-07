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

#include "settings.h"
#include "alike.h"

/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
 * Construct the default result
 *
 * `tar_pre` string to prepend before target
 * `target` what the object should be
 * `act_pre` string to prepend before actual
 * `actual` what the object is
 *
 * An example of the four strings:
 *
 * tar_pre: "be", target: "integer", act_pre: "is", actual: character
 *
 * This can then be assembled into "should be integer (is character)" or
 * potentially collapsed with other messages with `ALIKE_merge_msg`.
 */
SEXP ALIKEC_res_msg_def(
  const char * tar_pre, const char * target,
  const char * act_pre, const char * actual
) {
  SEXP res = PROTECT(allocVector(VECSXP, 2));
  SEXP res_msg = PROTECT(allocVector(STRSXP, 4));

  SET_STRING_ELT(res_msg, 0, mkChar(tar_pre));
  SET_STRING_ELT(res_msg, 1, mkChar(target));
  SET_STRING_ELT(res_msg, 2, mkChar(act_pre));
  SET_STRING_ELT(res_msg, 3, mkChar(actual));

  SET_VECTOR_ELT(res, 0, res_msg);                // message
  SET_VECTOR_ELT(res, 1, allocVector(VECSXP, 2)); // wrap

  SEXP res_names = PROTECT(allocVector(STRSXP, 2));
  SET_STRING_ELT(res_names, 0, mkChar("message"));
  SET_STRING_ELT(res_names, 1, mkChar("wrap"));

  setAttrib(res, R_NamesSymbol, res_names);
  UNPROTECT(3);

  return res;
}
/*
 * Create a SEXP out of an ALIKEC_res_strings struct
 */
SEXP ALIKEC_res_strings_to_SEXP(struct ALIKEC_res_strings strings) {
  SEXP res = PROTECT(allocVector(STRSXP, 4));
  SET_STRING_ELT(res, 0, mkChar(strings.tar_pre));
  SET_STRING_ELT(res, 1, mkChar(strings.target));
  SET_STRING_ELT(res, 2, mkChar(strings.act_pre));
  SET_STRING_ELT(res, 3, mkChar(strings.actual));
  UNPROTECT(1);
  return res;
}
/*
Other struct initialization functions
*/
struct ALIKEC_res_sub ALIKEC_res_sub_def() {
  return (struct ALIKEC_res_sub) {
    .success=1,
    .message=R_NilValue,
    .df=0
  };
}
struct ALIKEC_res ALIKEC_res_def() {
  return (struct ALIKEC_res) {
    .success=1,
    .message=R_NilValue, // so we don't need to protect yet
    .df=0,
    .rec=ALIKEC_rec_def()
  };
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Object Check

This will not recurse directly on recursive objects, though recursive attributes
will get recursed into.

Check:
- type
- length
- attributes
*/
struct ALIKEC_res ALIKEC_alike_obj(
  SEXP target, SEXP current, struct VALC_settings set
) {
  int is_df = 0, err_lvl = 6;
  SEXPTYPE tar_type, cur_type;

  int err = 0, err_attr = 0;
  const char * err_tok1, * err_tok2, * msg_tmp;
  err_tok1 = err_tok2 = msg_tmp = "";

  struct ALIKEC_res_strings err_fun;
  struct ALIKEC_res_fin err_type;
  struct ALIKEC_res res = ALIKEC_res_def();

  tar_type = TYPEOF(target);
  cur_type = TYPEOF(current);
  int s4_cur, s4_tar;
  s4_tar = ((IS_S4_OBJECT)(target) != 0);
  s4_cur = ((IS_S4_OBJECT)(current) != 0);

  if(!err && (s4_cur || s4_tar)) {  // don't run length or attribute checks on S4
    if(s4_tar + s4_cur == 1) {
      err = 1;
      const char * msg_tmp = CSR_smprintf4(
        set.nchar_max, "%sbe", (s4_tar ? "" : "not "), "", "", ""
      );
      res.message = PROTECT(ALIKEC_res_msg_def(msg_tmp, "S4", "", ""));
    } else {
      SEXP klass, klass_attrib;
      SEXP s, t;

      klass = getAttrib(target, R_ClassSymbol);
      if(xlength(klass) != 1 || TYPEOF(klass) != STRSXP) {
        // nocov start
        error(
          "Internal Error: unexpected S4 class \"class\" attribute %s%s",
          "of length != 1 or type not character vector; ",
          "contact package maintainer"
        );
        // nocov end
      }
      klass_attrib = getAttrib(klass, ALIKEC_SYM_package);
      if(xlength(klass_attrib) != 1 || TYPEOF(klass_attrib) != STRSXP) {
        // nocov start
        error(
          "Internal Error: unexpected S4 class \"class\" %s",
          "attribute does not have `package` attribute in expected structure"
        );
        // nocov end
      }

      // Construct call to `inherits`; we evaluate in base env since class
      // definitions should still be visible and this way unlikely that
      // inherits gets overwritten

      t = s = PROTECT(allocList(3));
      SET_TYPEOF(s, LANGSXP);
      SETCAR(t, ALIKEC_SYM_inherits); t = CDR(t);
      SETCAR(t, current); t = CDR(t);
      SETCAR(t, klass);
      int inherits = asLogical(eval(s, R_BaseEnv));
      UNPROTECT(1);

      if(!inherits) {
        err = 1;
        const char * msg_tmp = CSR_smprintf4(
          set.nchar_max, "S4 class \"%s\" (package: %s)",
          CHAR(asChar(klass)), CHAR(asChar(klass_attrib)), "", ""
        );
        res.message = PROTECT(
          ALIKEC_res_msg_def("inherit from", msg_tmp, "", "")
        );
      } else PROTECT(R_NilValue);
    }
    PROTECT(R_NilValue); // stack balance with next `else if`
  } else if(target != R_NilValue) {  // Nil objects match anything when nested
    // - Attributes ------------------------------------------------------------
    /*
    Attributes must be run first to figure out whether we are dealing with a
    data frame or some such, but other than that error priority is lowest unless
    it is a class error any attribute error will get over-written by subsequent
    errors (except class errors)
    */
    struct ALIKEC_res_sub res_attr = ALIKEC_compare_attributes_internal(
      target, current, set
    );
    PROTECT(res_attr.message);

    is_df = res_attr.df;
    err_lvl = res_attr.lvl;

    const char * msg_tar_pre= "";
    const char * msg_target = "";
    const char * msg_act_pre = "";
    const char * msg_actual = "";

    if(!res_attr.success) {
      // If top level error (class), make sure not overriden by others
      if(res_attr.lvl <= 2) {
        err = 1;
        res.message = res_attr.message;
      }
      else err_attr = 1;
    }
    // - Special Language Objects && Funs --------------------------------------

    int is_lang = 0;
    if(
      !err &&
      (
        is_lang = (
          (tar_type == LANGSXP || tar_type == SYMSXP) &&
          (cur_type == LANGSXP || cur_type == SYMSXP)
      ) )
    ) {
      struct ALIKEC_res_sub res_lang = ALIKEC_lang_alike_internal(
        target, current, set
      );
      PROTECT(res_lang.message);
      if(!res_lang.success) {
        err = 1;
        res.message = res_lang.message;
      }
    } else PROTECT(R_NilValue);
    int is_fun = 0;

    if(!err && (is_fun = isFunction(target) && isFunction(current))) {
      err_fun = ALIKEC_fun_alike_internal(target, current, set);
      if(err_fun.target[0]) {
        err = 1;
        msg_tar_pre = err_fun.tar_pre;
        msg_target = err_fun.target;
        msg_act_pre = err_fun.act_pre;
        msg_actual = err_fun.actual;
    } }
    // - Type ------------------------------------------------------------------

    // lang excluded because we can have symbol-lang comparisons that resolve
    //  to symbol symbol

    if(!err && !is_lang) {
      err_type = ALIKEC_type_alike_internal(target, current, R_NilValue, set);
      if(err_type.target[0]) {
        err = 1;
        msg_tar_pre = err_type.tar_pre;
        msg_target = err_type.target;
        msg_act_pre = err_type.act_pre;
        msg_actual = err_type.actual;
    } }
    // - Length ----------------------------------------------------------------

    /*
    Note length is not checked explicilty for language objects and functions
    since parens or dots allow for different length objects to be alike, and
    for environments since rules for alikeness are different for environments
    */

    if(!err && !is_lang && !is_fun && tar_type != ENVSXP) {
      SEXP tar_first_el, cur_first_el;
      R_xlen_t tar_len, cur_len, tar_first_el_len, cur_first_el_len;
      // if attribute error is not class, override with col count error
      // zero lengths match any length
      int err_tmp_1 = (!err || (is_df && err_lvl > 0));
      int err_tmp_2 = (tar_len = xlength(target)) > 0;
      if(
        err_tmp_1 && err_tmp_2 && tar_len != (cur_len = xlength(current))
      ) {
        err = 1;
        err_tok1 = CSR_len_as_chr(tar_len);
        err_tok2 = CSR_len_as_chr(cur_len);
        if(is_df) {
          msg_tar_pre = "have";
          msg_target = CSR_smprintf4(
            set.nchar_max, "%s column%s",
            err_tok1, tar_len == (R_xlen_t) 1 ? "" : "s", "",  ""
          );
          msg_act_pre = "has";
          msg_actual = CSR_smprintf4(
            set.nchar_max, "%s", err_tok2,  "", "", ""
          );
        } else {
          msg_tar_pre = "be";
          msg_target = CSR_smprintf4(
            set.nchar_max, "length %s", err_tok1,  "",  "", ""
          );
          msg_act_pre = "is";
          msg_actual = CSR_smprintf4(
            set.nchar_max, "%s", err_tok2,  "", "", ""
          );
        }
      } else if (
        is_df && err_lvl > 0 && tar_type == VECSXP && XLENGTH(target) &&
        TYPEOF(current) == VECSXP && XLENGTH(current) &&
        isVectorAtomic((tar_first_el = VECTOR_ELT(target, 0))) &&
        isVectorAtomic((cur_first_el = VECTOR_ELT(current, 0))) &&
        (tar_first_el_len = XLENGTH(tar_first_el)) && tar_first_el_len &&
        tar_first_el_len != (cur_first_el_len = XLENGTH(cur_first_el))
      ) {
        // check for row count error, note this isn't a perfect check since we
        // check the first column only

        err = 1;
        msg_tar_pre = "have";
        msg_target = CSR_smprintf4(
          set.nchar_max, "%s row%s",
          CSR_len_as_chr(tar_first_el_len),
          tar_first_el_len == (R_xlen_t) 1 ? "" : "s", "", ""
        );
        msg_act_pre = "has";
        msg_actual = CSR_smprintf4(
          set.nchar_max, "%s",
          CSR_len_as_chr(cur_first_el_len), "", "", ""
        );
    } }
    // If no normal, errors, use the attribute error

    if(!err && err_attr) {
      res.message = PROTECT(res_attr.message);
    } else if(err && msg_target[0]) {
      res.message = PROTECT(
        ALIKEC_res_msg_def(msg_tar_pre, msg_target, msg_act_pre, msg_actual)
      );
    } else {
      PROTECT(R_NilValue);
    }
  } else {
    PROTECT(PROTECT(PROTECT(R_NilValue)));
  }
  // - Known Limitations -------------------------------------------------------

  if(!set.suppress_warnings) {
    switch(tar_type) {
      case NILSXP:
      case LGLSXP:
      case INTSXP:
      case REALSXP:
      case CPLXSXP:
      case STRSXP:
      case VECSXP:
      case S4SXP:
      case ENVSXP:
      case LISTSXP:
      case LANGSXP:
      case CLOSXP:
      case BUILTINSXP:
      case SPECIALSXP:
      case EXPRSXP:
      case SYMSXP:
      case EXTPTRSXP:
      case WEAKREFSXP:
        break;
      default:
        warning(
          "`alike` behavior for objects of type \"%s\" is not well defined %s",
          type2char(tar_type),
          "and may change in the future"
        );
    }
  }
  res.df = is_df;
  if(err || err_attr) {
    res.success = 0;
  } else {
    res.success = 1;
  }
  UNPROTECT(3);
  return res;
}
/*
Utility functions for updating index lest for error reporting.  General logic
is to track depth of recursion, and when an error occurs, allocate enough
space for as many ALIKEC_index structs as there is recursion depth.
*/
/*
Handle recursive types; these include VECSXP, environments, and pair lists.

NOTE: do not recurse into environments that are part of attributes as otherwise
this setup may not prevent infinite recursion.
*/
struct ALIKEC_res ALIKEC_alike_rec(
  SEXP target, SEXP current, struct ALIKEC_rec_track rec,
  struct VALC_settings set
) {
  /*
  Recurse through various types of recursive structures.

  Side note: probably don't need to generate full error report unless we're on
  outermost `ALIKEC_alike_internal` call since we don't display the inner
  reports anyway, so could be a bit more efficient there.

  General logic here is to check object for alikeness; if not initialize index
  and return error structure, if so then recurse using specialized logic for
  each type of recursive structure.  After recursion, check if recursion
  failed and if so record current index.  Since this happens at every level of
  the recursion we can recreate the full index to the location of the error.
  */
  void R_CheckUserInterrupt(void);

  // normal logic, which will have checked length and attributes, etc.  Note
  // funny protection here, we PROTECT the message, and if we ever modify
  // res or res.message we UNPROTECT and reprotect to avoid growing the
  // PROTECT stack

  struct ALIKEC_res res = ALIKEC_alike_obj(target, current, set);

  PROTECT(res.message);
  res.rec = rec;

  if(!res.success) {
    res.rec.lvl_max = res.rec.lvl;
  } else {
    res.rec = ALIKEC_rec_inc(res.rec);  // Increase recursion level

    R_xlen_t tar_len = xlength(target);
    SEXPTYPE tar_type = TYPEOF(target);

    if(tar_type == VECSXP || tar_type == EXPRSXP) {
      R_xlen_t i;

      for(i = 0; i < tar_len; i++) {
        res = ALIKEC_alike_rec(
          VECTOR_ELT(target, i), VECTOR_ELT(current, i), res.rec, set
        );
        UNPROTECT(1);
        PROTECT(res.message);

        if(!res.success) {
          SEXP vec_names = getAttrib(target, R_NamesSymbol);
          const char * ind_name;
          if(
            vec_names == R_NilValue ||
            !((ind_name = CHAR(STRING_ELT(vec_names, i))))[0]
          )
            res.rec = ALIKEC_rec_ind_num(res.rec, i + 1);
          else
            res.rec = ALIKEC_rec_ind_chr(res.rec, ind_name);
          break;
        }
      }
    } else if (tar_type == ENVSXP && !set.in_attr) {
      // Need to guard against possible circular reference in the environments
      // Note it is important that we cannot recurse when checking environments
      // in attributes as othrewise we could get inifinite recursion since
      // rec tracking is specific to each call to ALIKEC_alike_internal

      if(!res.rec.envs) res.rec.envs =
        ALIKEC_env_set_create(16, set.env_depth_max);

      int env_stack_status =
        ALIKEC_env_track(target, res.rec.envs, set.env_depth_max);
      if(!res.rec.envs->no_rec)
        res.rec.envs->no_rec = !env_stack_status;
      if(env_stack_status  < 0 && !set.suppress_warnings) {
        warning(
          "`alike` environment stack exhausted at recursion depth %d; %s%s",
          set.env_depth_max,
          "unable to recurse any further into environments; see ",
          "`env.depth.max` parameter for `vetr_settings`."
        );
        res.rec.envs->no_rec = 1; // so we only get warning once
      }
      if(res.rec.envs->no_rec || target == current) {
        res.success = 1;
      } else {
        if(target == R_GlobalEnv && current != R_GlobalEnv) {
          res.success = 0;
          UNPROTECT(1);
          res.message =
            PROTECT(ALIKEC_res_msg_def("be", "the global environment", "", ""));
        } else {
          SEXP tar_names = PROTECT(R_lsInternal(target, TRUE));
          R_xlen_t tar_name_len = XLENGTH(tar_names), i;

          if(tar_name_len != tar_len) {
            // nocov start
            error(
              "Internal Error: mismatching name-env lengths; contact maintainer"
            );
            // nocov end
          }
          for(i = 0; i < tar_len; i++) {
            const char * var_name_chr = CHAR(STRING_ELT(tar_names, i));
            SEXP var_name = PROTECT(install(var_name_chr));
            SEXP var_cur_val = findVarInFrame(current, var_name);
            if(var_cur_val == R_UnboundValue) {
              res.success = 0;
              UNPROTECT(1);
              res.message=PROTECT(
                ALIKEC_res_msg_def(
                  "contain",
                  CSR_smprintf4(
                    set.nchar_max, "variable `%s`",
                    CHAR(asChar(STRING_ELT(tar_names, i))), "", "", ""
                  ),
                  "", ""
                )
              );
              UNPROTECT(1); // unprotect var_name
              break;
            } else {
              res = ALIKEC_alike_rec(
                findVarInFrame(target, var_name), var_cur_val, res.rec, set
              );
              UNPROTECT(1);
              PROTECT(res.message);

              UNPROTECT(1); // unprotect var_name
              if(!res.success) {
                res.rec = ALIKEC_rec_ind_chr(res.rec, var_name_chr);
                break;
          } } }
          UNPROTECT(1);
        }
      }
    } else if (tar_type == LISTSXP) {
      SEXP tar_sub, cur_sub;
      R_xlen_t i = 0;
      for(
        tar_sub = target, cur_sub = current; tar_sub != R_NilValue;
        tar_sub = CDR(tar_sub), cur_sub = CDR(cur_sub), i++
      ) {
        // Check tag names; should be in same order??  Probably

        SEXP tar_tag = TAG(tar_sub);
        SEXP tar_tag_chr = PRINTNAME(tar_tag);
        if(tar_tag != R_NilValue && tar_tag != TAG(cur_sub)) {
          UNPROTECT(1);
          res.message= PROTECT(
            ALIKEC_res_msg_def(
              "have",
              CSR_smprintf4(
                set.nchar_max, "name \"%s\" at pairlist index [[%s]]",
                CHAR(asChar(tar_tag_chr)), CSR_len_as_chr(i + 1), "", ""
              ),
              "", ""
            )
          );
          res.success = 0;
          break;
        } else {
          res = ALIKEC_alike_rec(CAR(tar_sub), CAR(cur_sub), res.rec, set);
          UNPROTECT(1);
          PROTECT(res.message);
          if(!res.success) {
            if(tar_tag != R_NilValue)
              res.rec =
                ALIKEC_rec_ind_chr(res.rec, CHAR(asChar(tar_tag_chr)));
            else
              res.rec =
                ALIKEC_rec_ind_num(res.rec, i + 1);
            break;
    } } } }
    res.rec = ALIKEC_rec_dec(res.rec); // decrement recursion tracker
  }
  UNPROTECT(1); // if we handled msg PROTECT properly stack should be 1 deep
  return res;
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Run alike calculation, and in particular, compose error message if relevant.

Return value is a character value that starts with %s and follows with
something like "should be ...".
*/
struct ALIKEC_res ALIKEC_alike_internal(
  SEXP target, SEXP current, struct VALC_settings set
) {
  if(set.type_mode < 0 || set.type_mode > 2)
    error("Interal Error: argument `type.mode` must be in 0:2");  // nocov
  if(set.attr_mode < 0 || set.attr_mode > 2)
    error("Interal Error: `attr.mode` must be in 0:2");           // nocov

  struct ALIKEC_res res = ALIKEC_res_def();

  // Note, no PROTECTion since we exit immediately (res.message is SEXP)

  if(TYPEOF(target) == NILSXP && TYPEOF(current) != NILSXP) {
    // Handle NULL special case at top level

    res.success = 0;
    res.message = ALIKEC_res_msg_def(
      "be", "\"NULL\"", "is", CSR_smprintf4(
      set.nchar_max, "\"%s\"", type2char(TYPEOF(current)), "", "", ""
    ) );
  } else {
    // Recursively check object

    res = ALIKEC_alike_rec(target, current, ALIKEC_rec_def(), set);
  }
  return res;
}
/*
Outermost alike function, handles full rendering including the leading
substituted expression

Note that width is only really used to control the deparse wrapping; rest of
text is not wrapped.  Negative width will use the getOption("width");

curr_sub is just current substituted
*/
struct ALIKEC_res_fin ALIKEC_alike_wrap(
  SEXP target, SEXP current, SEXP curr_sub, struct VALC_settings set
) {
  if(
    TYPEOF(curr_sub) != LANGSXP && TYPEOF(curr_sub) != SYMSXP &&
    !(isVectorAtomic(curr_sub) && XLENGTH(curr_sub) == 1) &&
    curr_sub != R_NilValue
  )
    error("Internal Error; `curr_sub` must be language."); // nocov

  struct ALIKEC_res res = ALIKEC_alike_internal(target, current, set);
  PROTECT(res.message);
  struct ALIKEC_res_fin res_out = {
    .tar_pre = "", .target="", .act_pre="", .actual="", .call = ""
  };
  // Have an error, need to populate the object by deparsing the relevant
  // expression.  One issue here is we want different treatment depending on
  // how wide the error is; if the error is short enough we can include it
  // inline; otherwise we need to modify how we display it

  if(!res.success) {
    // Get indices, and sub in the current substituted expression if they
    // exist

    res_out.tar_pre= CHAR(STRING_ELT(VECTOR_ELT(res.message, 0), 0));
    res_out.target = CHAR(STRING_ELT(VECTOR_ELT(res.message, 0), 1));

    res_out.act_pre = CHAR(STRING_ELT(VECTOR_ELT(res.message, 0), 2));
    res_out.actual = CHAR(STRING_ELT(VECTOR_ELT(res.message, 0), 3));

    SEXP rec_ind = PROTECT(ALIKEC_rec_ind_as_lang(res.rec));
    SEXP wrap = VECTOR_ELT(res.message, 1);

    // Need to check if our call could become ambigous with the indexing
    // element (e.g. `1 + x[[1]][[2]]` should be `(1 + x)[[1]][[2]]`

    // Condition is a wee bit sloppy, we're assuming that rec_ind is either an
    // operator or nothing, b/c if a function then we don't need to use parens
    // even if wrap is an op.  I'm pretty sure this is always true though.
    if(
      ALIKEC_is_an_op(curr_sub) &&
      (
        ALIKEC_is_an_op(VECTOR_ELT(rec_ind, 0)) ||
        ALIKEC_is_an_op_inner(VECTOR_ELT(wrap, 0))
      )
    ) {
      curr_sub = PROTECT(lang2(ALIKEC_SYM_paren_open, curr_sub));
    } else {
      PROTECT(R_NilValue);
    }
    // Extra recursion index needed: this should be applied before the wrap

    if(TYPEOF(VECTOR_ELT(rec_ind, 0)) == LANGSXP) {
      SETCAR(VECTOR_ELT(rec_ind, 1), curr_sub);
      curr_sub = VECTOR_ELT(rec_ind, 0);
    }
    // Merge the wrap call with the original call so we can get stuff like
    // `names(curr_sub)`

    if(VECTOR_ELT(wrap, 0) != R_NilValue) {
      SETCAR(VECTOR_ELT(wrap, 1), curr_sub);
      curr_sub = VECTOR_ELT(wrap, 0);
    }
    // Deparse and format the call

    res_out.call = ALIKEC_pad_or_quote(
      curr_sub, set.width,
      asLogical(getAttrib(rec_ind, ALIKEC_SYM_syntacticnames)), set
    );
    UNPROTECT(2);
  }
  UNPROTECT(1);
  return res_out;
}
/*
Main external interface
*/
SEXP ALIKEC_alike_ext(
  SEXP target, SEXP current, SEXP curr_sub, SEXP env, SEXP settings
) {
  if(
    TYPEOF(curr_sub) != LANGSXP && TYPEOF(curr_sub) != SYMSXP &&
    !(isVectorAtomic(curr_sub) && XLENGTH(curr_sub) == 1) &&
    curr_sub != R_NilValue
  ) {
    // nocov start
    error(
      "Internal Error; `curr_sub` must be language."
    );
    // nocov end
  }
  struct VALC_settings set = VALC_settings_vet(settings, env);
  return ALIKEC_string_or_true(
    ALIKEC_alike_wrap(target, current, curr_sub, set), set
  );
}
/*
 * Another secondary, but takes the set struct instead of SEXP list, and returns
 * length 5 character vectors for the errors instead of length 1 so that the
 * return values can be used with ALIKEC_merge_msg.
 */
SEXP ALIKEC_alike_int2(
  SEXP target, SEXP current, SEXP curr_sub, struct VALC_settings set
) {
  return ALIKEC_strsxp_or_true(
    ALIKEC_alike_wrap(target, current, curr_sub, set)
  );
}
