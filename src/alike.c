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
 * Create a SEXP out of an ALIKEC_res_strings struct
 *
 * See `ALIKEC_string_or_true` for related function.
 */
SEXP ALIKEC_res_strings_to_SEXP(struct ALIKEC_res_strings strings) {
  struct VALC_settings set = VALC_settings_init();
  struct ALIKEC_tar_cur_strings strings_pasted =
    ALIKEC_res_as_strings(strings, set);

  SEXP res = PROTECT(allocVector(STRSXP, 4));
  SET_STRING_ELT(res, 0, mkChar(strings.tar_pre));
  SET_STRING_ELT(res, 1, mkChar(strings_pasted.target));
  if(strings_pasted.current[0]) {
    SET_STRING_ELT(res, 2, mkChar(strings.cur_pre));
    SET_STRING_ELT(res, 3, mkChar(strings_pasted.current));
  } else {
    SET_STRING_ELT(res, 2, mkChar(""));
    SET_STRING_ELT(res, 3, mkChar(""));
  }
  UNPROTECT(1);
  return res;
}
/*
 * Other struct initialization functions, see alike.h for descriptions
 *
 * One question here is whether we should create this object once and then
 * re-use it unless an actual error occurs to avoid the R_alloc calls.
 */
struct ALIKEC_res_strings ALIKEC_res_strings_init() {
  struct ALIKEC_res_strings res;

  res.target = (const char **) R_alloc(5, sizeof(const char *));
  res.current = (const char **) R_alloc(5, sizeof(const char *));

  res.target[0] = "%s%s%s%s";
  res.target[1] = "";
  res.target[2] = "";
  res.target[3] = "";
  res.target[4] = "";

  res.tar_pre = "be";

  res.current[0] = "%s%s%s%s";
  res.current[1] = "";
  res.current[2] = "";
  res.current[3] = "";
  res.current[4] = "";

  res.cur_pre = "is";

  return res;
}
struct ALIKEC_res ALIKEC_res_init() {
  return (struct ALIKEC_res) {
    .success=1,
    .strings=ALIKEC_res_strings_init(),
    .rec=ALIKEC_rec_track_init(),
    // Would be cleaner to initialize this to allocVector(VECSXP, 2), but that
    // costs ~20ns and we do that a lot so instead we initialize this as
    // R_NilValue and rely on code to do the correct initialization when
    // something actually fails.  Dirty, but performance impact seems to warrant
    // it as we can easily instantiate dozens of these objects.
    .wrap=R_NilValue,
    .df=0,
    .lvl=0
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
  SEXPTYPE tar_type, cur_type;

  const char * err_tok1, * err_tok2, * msg_tmp;
  err_tok1 = err_tok2 = msg_tmp = "";

  struct ALIKEC_res res = ALIKEC_res_init();
  res.df = 0;
  res.lvl = 6;

  tar_type = TYPEOF(target);
  cur_type = TYPEOF(current);
  int s4_cur, s4_tar;
  s4_tar = ((IS_S4_OBJECT)(target) != 0);
  s4_cur = ((IS_S4_OBJECT)(current) != 0);

  // don't run length or attribute checks on S4
  if(!res.success && (s4_cur || s4_tar)) {
    if(s4_tar + s4_cur == 1) {
      res.success = 0;
      res.strings.tar_pre = s4_tar ? "be" : "not be";
      res.strings.target[1] = "S4";
    } else {
      SEXP klass, klass_attrib;

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
      const char * klass_c = CHAR(asChar(klass));
      // We used to construct an inherits call here; not entirely clear why we
      // did this instead of just using Rf_inherits

      if(!inherits(current, klass_c)) {
        res.success = 0;
        res.strings.tar_pre = "inherit from";

        klass_attrib = getAttrib(klass, ALIKEC_SYM_package);
        if(xlength(klass_attrib) != 1 || TYPEOF(klass_attrib) != STRSXP) {
          // nocov start
          error(
            "Internal Error: unexpected S4 class \"class\" %s",
            "attribute does not have `package` attribute in expected structure"
          );
          // nocov end
        }
        res.strings.target[0] = "S4 class \"%s\" (package: %s)";
        res.strings.target[1] = klass_c;
        res.strings.target[2] = CHAR(asChar(klass_attrib));
      }
    }
    PROTECT(PROTECT(R_NilValue)); // stack balance with next `else if`
  } else if(target != R_NilValue) {  // Nil objects match anything when nested
    // - Attributes ------------------------------------------------------------
    /*
    Attributes must be run first to figure out whether we are dealing with a
    data frame or some such, but other than that error priority is lowest unless
    it is a class error any attribute error will get over-written by subsequent
    errors (except class errors)
    */
    struct ALIKEC_res res_attr = ALIKEC_compare_attributes_internal(
      target, current, set
    );
    PROTECT(res_attr.wrap);

    // All the other attributes we keep overwriting the results of; to simplify
    // protection logic we create a dummy PROTECT here for stack balance (see
    // next UNPROTECT for why)

    PROTECT(R_NilValue);

    if(!res_attr.success) {
      // If top level error (class), make sure not overriden by others so make
      // it a overall error instead of just and attribute error

      if(res_attr.lvl <= 2)  res = res_attr;
    }
    // - Special Language Objects && Funs --------------------------------------

    int is_lang = 0;
    if(
      !res.success &&
      (
        is_lang = (
          (tar_type == LANGSXP || tar_type == SYMSXP) &&
          (cur_type == LANGSXP || cur_type == SYMSXP)
      ) )
    ) {
      UNPROTECT(1);
      res = ALIKEC_lang_alike_internal(target, current, set);
      PROTECT(res.wrap);
    }
    int is_fun = 0;

    if(res.success && (is_fun = isFunction(target) && isFunction(current))) {
      UNPROTECT(1);
      res = ALIKEC_fun_alike_internal(target, current, set);
      PROTECT(res.wrap);
    }
    // - Type ------------------------------------------------------------------

    // lang excluded because we can have symbol-lang comparisons that resolve
    //  to symbol symbol

    if(res.success && !is_lang) {
      UNPROTECT(1);
      res = ALIKEC_type_alike_internal(target, current, R_NilValue, set);
      PROTECT(res.wrap);
    }
    // - Length ----------------------------------------------------------------
    /*
    Note length is not checked explicilty for language objects and functions
    since parens or dots allow for different length objects to be alike, and
    for environments since rules for alikeness are different for environments
    */

    if(res.success && !is_lang && !is_fun && tar_type != ENVSXP) {
      SEXP tar_first_el, cur_first_el;
      R_xlen_t tar_len, cur_len, tar_first_el_len, cur_first_el_len;
      // if attribute error is not class, override with col count error
      // zero lengths match any length
      int err_tmp_1 = (res.success || (res.df && res.lvl > 0));
      int err_tmp_2 = (tar_len = xlength(target)) > 0;
      if(
        err_tmp_1 && err_tmp_2 && tar_len != (cur_len = xlength(current))
      ) {
        res.success = 0;
        err_tok1 = CSR_len_as_chr(tar_len);
        err_tok2 = CSR_len_as_chr(cur_len);
        res.strings.target[1] = err_tok1;
        res.strings.current[1] = err_tok2;

        if(res.df) {
          res.strings.tar_pre = "have";
          res.strings.target[0] = "%s column%s";
          res.strings.target[2] = tar_len == (R_xlen_t) 1 ? "" : "s";
          res.strings.cur_pre = "has";
        } else {
          // Update this to use wrap??
          res.strings.tar_pre = "be";
          res.strings.target[0] = "length %s";
          res.strings.cur_pre = "is";
        }
      } else if (
        res.df && res.lvl > 0 && tar_type == VECSXP && XLENGTH(target) &&
        TYPEOF(current) == VECSXP && XLENGTH(current) &&
        isVectorAtomic((tar_first_el = VECTOR_ELT(target, 0))) &&
        isVectorAtomic((cur_first_el = VECTOR_ELT(current, 0))) &&
        (tar_first_el_len = XLENGTH(tar_first_el)) && tar_first_el_len &&
        tar_first_el_len != (cur_first_el_len = XLENGTH(cur_first_el))
      ) {
        // check for row count error, note this isn't a perfect check since we
        // check the first column only

        res.success = 0;
        res.strings.tar_pre = "have";
        res.strings.target[0] = "%s row%s";
        res.strings.target[1] = CSR_len_as_chr(tar_first_el_len);
        res.strings.target[1] = tar_first_el_len == (R_xlen_t) 1 ? "" : "s";
        res.strings.cur_pre = "has";
        res.strings.current[2] = CSR_len_as_chr(cur_first_el_len);
    } }
    // If no normal, errors, use the attribute error

    if(res.success && !res_attr.success) {
      res = res_attr;
    }
  } else {
    PROTECT(PROTECT(R_NilValue));
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
  UNPROTECT(2);
  if(!res.success && res.wrap == R_NilValue) {
    res.wrap = allocVector(VECSXP, 2);
  }
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
  res.rec = rec;

  if(!res.success) {
    PROTECT(res.wrap);  // failed wraps must be protected
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
        if(!res.success) {
          PROTECT(res.wrap);  // failed wrap must be protected
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
          res.strings.tar_pre = "be";
          res.strings.target[1] =  "the global environment";
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
              res.strings.tar_pre = "contain";
              res.strings.target[0] = "variable `%s`";
              res.strings.target[1] = var_name_chr;
              UNPROTECT(1); // unprotect var_name
              break;
            } else {
              res = ALIKEC_alike_rec(
                findVarInFrame(target, var_name), var_cur_val, res.rec, set
              );
              UNPROTECT(1); // unprotect var_name, assume no GC...
              PROTECT(res.wrap);
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
          UNPROTECT(1);  // don't need wrap anymore
          res.success = 0;
          res.strings.tar_pre = "have";
          res.strings.target[0] =  "name \"%s\" at pairlist index [[%s]]";
          res.strings.target[0] =  CHAR(asChar(tar_tag_chr));
          res.strings.target[0] =  CSR_len_as_chr(i + 1);
          break;
        } else {
          UNPROTECT(1);
          res = ALIKEC_alike_rec(CAR(tar_sub), CAR(cur_sub), res.rec, set);
          PROTECT(res.wrap);
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

  if(!res.success && res.wrap == R_NilValue) res.wrap=allocVector(VECSXP, 2);
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

  struct ALIKEC_res res = ALIKEC_res_init();

  if(TYPEOF(target) == NILSXP && TYPEOF(current) != NILSXP) {
    // Handle NULL special case at top level

    res.success = 0;
    res.strings.target[1] = "\"NULL\"";
    res.strings.current[0] = "\"%s\"";
    res.strings.current[1] = type2char(TYPEOF(current));
    res.wrap = PROTECT(allocVector(VECSXP, 2));
  } else {
    // Recursively check object

    res = ALIKEC_alike_rec(target, current, ALIKEC_rec_track_init(), set);
    PROTECT(R_NilValue);  /// stack balance
  }
  UNPROTECT(1);
  return res;
}
/*
 * Augments wrap by injection call in the reserved spot
 *
 * This whole wrap business is needed because we do not generate the recursion
 * indices until we get here, so we need a mechanism for generating languge of
 * the form
 *
 *    attr(xxx[[1]][[2]])
 *
 * We have `xxx` from the get go, but when we generate the wrap (e.g.
 * `attr(...)`) we cannot / do not want to generate the index yet.  The wrap
 * business allows us to inject the stuff in later (here currently).  Part of
 * the reason we don't know the index is that we retrieve this data as we exit
 * from the recursion.  I guess it probably would be possible to just do it from
 * the deepest point in the recursion, but seemed easier to naturally collect it
 * as the recursion unwinds that way we only collect on the branch of the
 * recursion that we actually need it for (I guess it's implicitly always
 * hanging out in the call stack, but whatver).
 *
 * If there is no wrap element present, then we create a new one with the call.
 * The returned wrap needs to be protected as it may be different from the input
 * wrap.
 */
SEXP ALIKEC_inject_call(struct ALIKEC_res res, SEXP call) {
  SEXP rec_ind = PROTECT(ALIKEC_rec_ind_as_lang(res.rec));

  if(TYPEOF(res.wrap) != VECSXP || xlength(res.wrap) != 2) {
    error("Internal Error: wrap struct eleme should be length 2 list.");
  }
  SEXP wrap = res.wrap;

  // Need to check if our call could become ambigous with the indexing
  // element (e.g. `1 + x[[1]][[2]]` should be `(1 + x)[[1]][[2]]`

  // Condition is a wee bit sloppy, we're assuming that rec_ind is either an
  // operator or nothing, b/c if a function then we don't need to use parens
  // even if wrap is an op.  I'm pretty sure this is always true though.

  if(
    ALIKEC_is_an_op(call) &&
    (
      ALIKEC_is_an_op(VECTOR_ELT(rec_ind, 0)) ||
      ALIKEC_is_an_op_inner(VECTOR_ELT(wrap, 0))
    )
  ) {
    call = PROTECT(lang2(ALIKEC_SYM_paren_open, call));
  } else {
    PROTECT(R_NilValue);
  }
  // Extra recursion index needed: this should be applied before the wrap

  if(TYPEOF(VECTOR_ELT(rec_ind, 0)) == LANGSXP) {
    SETCAR(VECTOR_ELT(rec_ind, 1), call);
    call = VECTOR_ELT(rec_ind, 0);
  }
  // Merge the wrap call with the original call so we can get stuff like
  // `names(call)`

  if(
    VECTOR_ELT(wrap, 0) != R_NilValue && TYPEOF(VECTOR_ELT(wrap, 1)) == LANGSXP
  ) {
    SETCAR(VECTOR_ELT(wrap, 1), call);
  } else {
    SET_VECTOR_ELT(wrap, 0, call);
  }
  UNPROTECT(2);
  return VECTOR_ELT(wrap, 0);
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
  struct ALIKEC_res res = ALIKEC_alike_internal(target, current, set);
  PROTECT(res.wrap);
  SEXP res_sxp = PROTECT(ALIKEC_string_or_true(res, curr_sub, set));
  UNPROTECT(2);
  return res_sxp;
}
/*
 * Another secondary, but takes the set struct instead of SEXP list, and returns
 * length 5 character vectors for the errors instead of length 1 so that the
 * return values can be used with ALIKEC_merge_msg.
 *
 * Intended primarily for use by vetr
 */
SEXP ALIKEC_alike_int2(
  SEXP target, SEXP current, SEXP curr_sub, struct VALC_settings set
) {
  struct ALIKEC_res res = ALIKEC_alike_internal(target, current, set);
  PROTECT(res.wrap);  // might have been changed by inject
  SEXP res_sxp = PROTECT(ALIKEC_strsxp_or_true(res, curr_sub, set));
  UNPROTECT(2);
  return res_sxp;
}
