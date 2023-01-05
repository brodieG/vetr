/*
Copyright (C) 2023 Brodie Gaslam

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

#include "alike.h"
/*
 * used to take res_sub as input, but we got rid of that when we rationalized
 * most of our result structs to be ALIKEC_res
 *
 * We engage in some contortions here to recreate what the original object would
 * have looked like so that our tests still pass
 */

SEXP ALIKEC_res_sub_as_sxp(struct ALIKEC_res sub, struct VALC_settings set) {
  PROTECT(sub.wrap);  // not sure why we did this previously
  SEXP out = PROTECT(allocVector(VECSXP, 4));
  SEXP out_names = PROTECT(allocVector(STRSXP, 4));
  const char * names[4] = {"success", "message", "df", "lvl"};
  for(int i = 0; i < 4; i++) SET_STRING_ELT(out_names, i, mkChar(names[i]));

  SEXP message;

  if(!sub.success) {
    struct ALIKEC_tar_cur_strings strings_pasted =
      ALIKEC_get_res_strings(sub.dat.strings, set);
    SEXP message_strings = PROTECT(allocVector(STRSXP, 4));
    if(strings_pasted.target[0]) {
      SET_STRING_ELT(message_strings, 0, mkChar(sub.dat.strings.tar_pre));
      SET_STRING_ELT(message_strings, 1, mkChar(strings_pasted.target));
    }
    if(strings_pasted.current[0]) {
      SET_STRING_ELT(message_strings, 2, mkChar(sub.dat.strings.cur_pre));
      SET_STRING_ELT(message_strings, 3, mkChar(strings_pasted.current));
    }
    message = PROTECT(allocVector(VECSXP, 2));
    SEXP res_msg_names = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(res_msg_names, 0, mkChar("message"));
    SET_STRING_ELT(res_msg_names, 1, mkChar("wrap"));
    setAttrib(message, R_NamesSymbol, res_msg_names);
    UNPROTECT(1);

    SET_VECTOR_ELT(message, 0, message_strings);
    // For coherence with older tests before we changed result structure
    if(sub.wrap == R_NilValue) {
      sub.wrap = PROTECT(allocVector(VECSXP, 2));
    } else PROTECT(R_NilValue);
    SET_VECTOR_ELT(message, 1, sub.wrap);
  } else message = PROTECT(PROTECT(PROTECT(R_NilValue)));

  SET_VECTOR_ELT(out, 0, ScalarInteger(sub.success));
  SET_VECTOR_ELT(out, 1, message);
  SET_VECTOR_ELT(out, 2, ScalarInteger(sub.dat.df));
  SET_VECTOR_ELT(out, 3, ScalarInteger(sub.dat.lvl));
  setAttrib(out, R_NamesSymbol, out_names);
  UNPROTECT(6);

  return out;
}
/*
Utility function to make a wrap sexp like `names(call)` when none exists
already; uses the symbol if it is one known to have an accessor function,
otherwise `attr(call, "x")`.
*/
SEXP ALIKEC_attr_wrap(SEXP tag, SEXP call) {
  if(TYPEOF(tag) != SYMSXP) error("attr_wrap only valid with tags");
  SEXP wrap = PROTECT(allocVector(VECSXP, 2));
  // Tags with accessor functions

  if(
    tag == R_NamesSymbol || tag == R_ClassSymbol || tag == R_TspSymbol ||
    tag == R_RowNamesSymbol || tag == R_DimNamesSymbol || tag == R_DimSymbol ||
    tag == R_LevelsSymbol
  ) {
    SET_VECTOR_ELT(wrap, 0, lang2(tag, call));
  } else {
    SEXP tag_name = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(tag_name, 0, PRINTNAME(tag));
    SEXP attr_call = PROTECT(lang3(ALIKEC_SYM_attr, call, tag_name));
    SET_VECTOR_ELT(wrap, 0, attr_call);
    UNPROTECT(2);
  }
  SET_VECTOR_ELT(wrap, 1, CDR(VECTOR_ELT(wrap, 0)));
  UNPROTECT(1);
  return wrap;
}
/*
Take an existing wrap and insert another wrapping call inside the wrap

Note this always assumes that the ultimate target of the wrapping is the CDR of
`call`

This function modifies `wrap` and does not return
*/
void ALIKEC_wrap_around(SEXP wrap, SEXP call) {
  if(TYPEOF(wrap) != VECSXP && xlength(wrap) != 2)
    error("Internal Error: Unexpected format for wrap object");  // nocov
  SEXP w1 = VECTOR_ELT(wrap, 0);
  SEXP w2 = VECTOR_ELT(wrap, 1);
  if(w1 != R_NilValue && TYPEOF(w1) != LANGSXP)
    // nocov start
    error(
      "%s%s",
      "Internal Error: First element of wrap object ",
      "must be NULL or  language"
    );
    // nocov end

  if(w1 == R_NilValue) {
    // unintialized
    SET_VECTOR_ELT(wrap, 0, call);
  } else {
    SETCAR(w2, call);
  }
  SET_VECTOR_ELT(wrap, 1, CDR(call));
}
/*
Runs alike on an attribute, really just like running alike, but since it is on
an attribute and we don't want a massive nested error message, provide a
different error message; this is not very efficient; in theory we could just
stop recursion since we're not returning the nested error message.

- `special` parameter indicates attributes that are known to have accessor
  functions (e.g. `names`).

We removed the `attr_attr` arg when we switched to settings.  Everything seems
to still work but it is worth noting that we were manually setting that variable
in calls to this fun in this file to either 0 or 1 depending on case, so it is
possible that we broke the treatment of some attributes.  Leaving these docs in
case we did do that and end up trying to figure out what happened.
- `attr_attr` indicates we are checking the attributes of an attribute; NOTE:
  can currently no longer remember how/why this should be used, we used to
  use `attr` when this was TRUE, and `attributes` when not, but that doesn't
  make much sense?
*/

struct ALIKEC_res ALIKEC_alike_attr(
  SEXP target, SEXP current, const char * attr_symb, struct VALC_settings set
) {
  struct ALIKEC_res res = ALIKEC_alike_internal(target, current, set);
  struct ALIKEC_res res_sub = ALIKEC_res_init();

  if(!res.success) {
    res_sub.success = 0;
    res_sub.dat.strings.tar_pre = "be";
    res_sub.dat.strings.target[1] =
      "`alike` the corresponding element in target";
    res_sub.dat.strings.current[1] = ""; // gcc-10

    SEXP attr_symb_sym = PROTECT(install(attr_symb));
    res_sub.wrap = PROTECT(ALIKEC_attr_wrap(attr_symb_sym, R_NilValue));
    UNPROTECT(2);
  }
  return res_sub;
}

/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Compare class attribute

Note that expectation is that unclassed objects will get sent here with their
implicit class defined by ALIKEC_mode.

Will set tar_is_df to 1 if prim is data frame
*/
struct ALIKEC_res ALIKEC_compare_class(
  SEXP target, SEXP current, struct VALC_settings set
) {
  if(TYPEOF(current) != STRSXP || TYPEOF(target) != STRSXP) {
    return ALIKEC_alike_attr(target, current, "class", set);
  }

  int tar_class_len, cur_class_len, len_delta, tar_class_i, cur_class_i,
      is_df = 0, idx_fail = -1;
  const char * cur_class = "<UNINITSTRING>", * cur_class_fail = "";
  const char * tar_class = "<UNINITSTRING>", * tar_class_fail = "";
  struct ALIKEC_res res = ALIKEC_res_init();

  tar_class_len = XLENGTH(target);
  cur_class_len = XLENGTH(current);
  R_xlen_t class_stop =
    tar_class_len > cur_class_len ? cur_class_len : tar_class_len;

  len_delta = cur_class_len - class_stop;

  for(
    cur_class_i = len_delta, tar_class_i = 0;
    cur_class_i < cur_class_len;
    cur_class_i++, tar_class_i++
  ) {
    cur_class = CHAR(STRING_ELT(current, cur_class_i));
    tar_class = CHAR(STRING_ELT(target, tar_class_i));
    if(!is_df && !strcmp(tar_class, "data.frame")) is_df = 1;

    // Only enter on first class mismatch, so protectins only happen once

    if(res.success && strcmp(cur_class, tar_class)) { // class mismatch

      res.success = 0;
      idx_fail = cur_class_i;
      tar_class_fail = tar_class;
      cur_class_fail = cur_class;
  } }
  // Check to make sure have enough classes

  if(!res.success) {
    if(cur_class_len > 1) {
      SEXP class_call = PROTECT(lang2(R_ClassSymbol, R_NilValue));
      SEXP sub_idx = PROTECT(ScalarReal(idx_fail + 1));
      SEXP wrap_call = PROTECT(lang3(R_BracketSymbol, class_call, sub_idx));
      SEXP wrap = PROTECT(allocVector(VECSXP, 2));
      SET_VECTOR_ELT(wrap, 0, wrap_call);
      SET_VECTOR_ELT(wrap, 1, CDR(CADR(wrap_call)));

      res.wrap=wrap;
      res.dat.strings.target[0] = "\"%s\"%s%s%s";
      res.dat.strings.target[1] = tar_class_fail;

      res.dat.strings.current[0] = "\"%s\"%s%s%s";
      res.dat.strings.current[1] = cur_class_fail;
    } else {
      res.dat.strings.target[0] = "class \"%s\"%s%s%s";
      res.dat.strings.target[1] = tar_class;

      res.dat.strings.current[0] = "\"%s\"%s%s%s";
      res.dat.strings.current[1] = cur_class;

      PROTECT(PROTECT(PROTECT(PROTECT(R_NilValue))));  // stack balance
    }
  } else {
    // stack balance for above
    PROTECT(PROTECT(PROTECT(PROTECT(R_NilValue))));

    if(tar_class_len > cur_class_len) {
      res.success = 0;

      res.dat.strings.tar_pre = "inherit";
      res.dat.strings.target[0] = "from class \"%s\"";
      res.dat.strings.target[1] = CHAR(STRING_ELT(target, tar_class_i));
      res.dat.strings.current[1] = ""; // gcc-10
    }
  }
  // Make sure class attributes are alike

  if(res.success) {
    res =
      ALIKEC_alike_attr(ATTRIB(target), ATTRIB(current), "class", set);
    PROTECT(res.wrap);
  } else PROTECT(R_NilValue);
  res.dat.df = is_df;
  UNPROTECT(5);
  return res;
}
SEXP ALIKEC_compare_class_ext(SEXP target, SEXP current) {
  struct VALC_settings set = VALC_settings_init();
  struct ALIKEC_res res = ALIKEC_compare_class(target, current, set);
  PROTECT(res.wrap);
  SEXP res_sxp = PROTECT(ALIKEC_res_sub_as_sxp(res, set));
  UNPROTECT(2);
  return res_sxp;
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Compares dimensions, but detects implicit classes by checking if atomic and
having dimensions and reports error as such if that is the case.

If there is an implicit class error res.lvl will be set to 1

tar_obj and cur_obj are the objects the dimensions are the attributes off.
*/
struct ALIKEC_res ALIKEC_compare_dims(
  SEXP target, SEXP current, SEXP tar_obj, SEXP cur_obj,
  struct VALC_settings set
) {
  // Invalid dims

  if(
    (TYPEOF(target) != INTSXP && target != R_NilValue) ||
    (TYPEOF(current) != INTSXP && current != R_NilValue)
  )
    return ALIKEC_alike_attr(target, current, "dim", set);

  // Dims -> implicit class

  R_xlen_t target_len = xlength(target), target_len_cap;
  target_len_cap = target_len > (R_xlen_t) 3 ? (R_xlen_t) 3 : target_len;
  R_xlen_t current_len = xlength(current), current_len_cap;
  current_len_cap = current_len > (R_xlen_t) 3 ? (R_xlen_t) 3 : current_len;

  struct ALIKEC_res res = ALIKEC_res_init();
  const char * class_err_target = "";
  const char * class_err_actual = "";

  if(target_len_cap > 1 && isVectorAtomic(tar_obj)) {
    if(current == R_NilValue) {  // current should be matrix/array
      class_err_target = target_len_cap > 2 ? "array" : "matrix";
      class_err_actual = ALIKEC_mode_int(cur_obj);
    } else if(isVectorAtomic(cur_obj) && current_len_cap != target_len_cap) {
      // target is matrix/array
      class_err_target = target_len_cap > 2 ? "array" : "matrix";
      class_err_actual = current_len_cap == 2 ?
        "matrix" : (current_len_cap == 1 ? "vector" : "array");
    } else if(!isVectorAtomic(cur_obj)) {
      // In this case, target is atomic, but current is not, would normally be
      // caught by earlier type comparisons so shouldn't get here unless testing
      // explicitly
      class_err_target = ALIKEC_mode_int(tar_obj);
      class_err_actual = type2char(TYPEOF(cur_obj));
    }
  } else if (current_len_cap > 1 && isVectorAtomic(cur_obj)) {
    if(isVectorAtomic(tar_obj)) {
      class_err_target = ALIKEC_mode_int(tar_obj);
      class_err_actual = current_len_cap == 2 ? "matrix" : "array";
    } else {
      class_err_target = ALIKEC_mode_int(tar_obj);
      class_err_actual = ALIKEC_mode_int(cur_obj);
    }
  }
  if(class_err_target[0]) {
    res.success = 0;
    res.dat.lvl = 1;
    res.dat.strings.target[0] = "\"%s\"%s%s%s";
    res.dat.strings.target[1] = class_err_target;

    res.dat.strings.current[0] = "\"%s\"%s%s%s";
    res.dat.strings.current[1] = class_err_actual;

    return res;
  }
  // Normal dim checking

  if(current == R_NilValue) {
    res.success = 0;
    res.dat.strings.tar_pre = "have";
    res.dat.strings.target[1] = "a \"dim\" attribute";
    res.dat.strings.current[1] = ""; // gcc-10

    return res;
  }
  if(target_len != current_len) {
    res.success = 0;
    res.dat.strings.tar_pre = "have";
    res.dat.strings.target[0] = "%s dimension%s%s%s";
    res.dat.strings.target[1] = CSR_len_as_chr(target_len);
    res.dat.strings.target[2] = target_len == (R_xlen_t) 1 ? "" : "s";

    res.dat.strings.cur_pre = "has";
    res.dat.strings.current[1] = CSR_len_as_chr(current_len);
    return res;
  }
  R_xlen_t attr_i;
  int tar_dim_val;

  for(attr_i = (R_xlen_t)0; attr_i < target_len; attr_i++) {
    tar_dim_val = INTEGER(target)[attr_i];
    const char * tar_dim_chr = CSR_len_as_chr((R_xlen_t)tar_dim_val);
    char * err_dim2;

    if(tar_dim_val && tar_dim_val != INTEGER(current)[attr_i]) {
      res.success = 0;
      res.dat.strings.tar_pre = "have";
      res.dat.strings.cur_pre = "has";
      // see below for res.strings.target
      res.dat.strings.current[1] =
        CSR_len_as_chr((R_xlen_t)(INTEGER(current)[attr_i]));

      if(target_len == 2) {  // Matrix
        switch(attr_i) {
          case (R_xlen_t) 0:
            err_dim2 = tar_dim_val == 1 ? " row" : " rows"; break;
          case (R_xlen_t) 1:
            err_dim2 = tar_dim_val == 1 ? " column" : " columns"; break;
            // nocov start
          default:
            error(
              "%s%s",
              "Internal Error: inconsistent matrix dimensions; contact  ",
              "maintainer."
            );
            // nocov end
        }
        res.dat.strings.target[1] = tar_dim_chr;
        res.dat.strings.target[2] = err_dim2;
      } else {
        res.dat.strings.target[0] = "size %s at dimension %s%s%s";
        res.dat.strings.target[1] = tar_dim_chr;
        res.dat.strings.target[2] = CSR_len_as_chr((R_xlen_t)(attr_i + 1));
      }
      return res;
  } }
  return ALIKEC_alike_attr(target, current, "dim", set);
}
SEXP ALIKEC_compare_dim_ext(
  SEXP target, SEXP current, SEXP tar_obj, SEXP cur_obj
) {
  struct VALC_settings set = VALC_settings_init();
  struct ALIKEC_res res =
    ALIKEC_compare_dims(target, current, tar_obj, cur_obj, set);
  PROTECT(res.wrap);
  SEXP res_sexp = PROTECT(ALIKEC_res_sub_as_sxp(res, set));
  UNPROTECT(2);
  return res_sexp;
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
check that an attribute could be `names`, `rownames` etc base on attributes
*/
int ALIKEC_are_special_char_attrs_internal(SEXP target, SEXP current) {
  SEXPTYPE cur_type, tar_type;
  R_xlen_t tar_len;
  tar_type = TYPEOF(target);
  cur_type = TYPEOF(current);
  tar_len = XLENGTH(target);

  return tar_type == cur_type && (tar_type == STRSXP || tar_type == INTSXP) &&
    tar_len == XLENGTH(current);
}
/*
Implements comparing character vectors element for element:
  - allowing zero length strings in `target` to match any length string in `current`
  - zero length `target` to match any `current`

This is used to compare names, row.names, etc.

Return value is either a zero length string if comparison successfull, or a
string containing `%s` that can then be used to snprintf in the name of the
object being compared.
*/
/*
Used to construct messages like:

`names(object[[1]]$a)[1]` should be "cat" (is "rat")
an underlying assumption is that the only attributes that end up coming
here are the special ones that have accessor functions.

Note that the calling function is responsible for handling parens so as to
allow for stuff like: `names(dimnames(object))` and of subbing in the attribute
names.
*/
struct ALIKEC_res ALIKEC_compare_special_char_attrs_internal(
  SEXP target, SEXP current, struct VALC_settings set, int strict
) {
  struct ALIKEC_res res = ALIKEC_alike_internal(target, current, set);
  PROTECT(res.wrap);
  struct ALIKEC_res res_sub = ALIKEC_res_init();

  // Dummy PROTECT since we will protect in only one of the branches and we
  // need this for stack balance (see next UNPROTECT)

  PROTECT(R_NilValue);

  // Special character attributes must be alike; not sure the logic here is
  // completely correct, will have to verify

  if(!res.success) {
    res_sub = res;
  } else {
    // But also have constraints on values

    SEXPTYPE cur_type = TYPEOF(current), tar_type = TYPEOF(target);
    R_xlen_t cur_len, tar_len, i;

    // should have been handled previously
    if(tar_type != cur_type) error("Internal Error 266");  // nocov
    else if (!(tar_len = XLENGTH(target))) {
      // zero len match to anything
    } else if ((cur_len = XLENGTH(current)) != tar_len) {
      // should have been handled previously
      error("Internal Error 268");   // nocov
    } else if (tar_type == INTSXP) {
      if(!R_compute_identical(target, current, 16)){
        res_sub.success = 0;
        res_sub.dat.strings.target[1] = "identical to target";
        res_sub.dat.strings.current[1] = ""; // gcc-10
      }
    } else if (tar_type == STRSXP) {
      // Only determine what name is wrong if we know there is a mismatch since
      // we have to loop thorugh each value.  Zero length targets match anything
      // unless in strict mode

      if(!R_compute_identical(target, current, 16)) {
        for(i = (R_xlen_t) 0; i < tar_len; i++) {
          const char * cur_name_val = CHAR(STRING_ELT(current, i));
          const char * tar_name_val = CHAR(STRING_ELT(target, i));
          if(         // check dimnames names match
            (strict || tar_name_val[0]) &&
            strcmp(tar_name_val, cur_name_val) != 0
          ) {
            UNPROTECT(1);  // undo dummy protect
            res_sub.success=0;
            res_sub.dat.strings.target[0] = "\"%s\"%s%s%s";
            res_sub.dat.strings.target[1] = tar_name_val;
            res_sub.dat.strings.current[0] = "\"%s\"%s%s%s";
            res_sub.dat.strings.current[1] = cur_name_val;

            res_sub.wrap = PROTECT(allocVector(VECSXP, 2));
            SEXP sub_ind = PROTECT(ScalarReal(i + 1));
            SEXP wrap_ind = PROTECT(lang3(R_BracketSymbol, R_NilValue, sub_ind));
            SET_VECTOR_ELT(res_sub.wrap, 0, wrap_ind);
            UNPROTECT(2);
            SET_VECTOR_ELT(res_sub.wrap, 1, CDR(VECTOR_ELT(res_sub.wrap, 0)));
            break;
      } } }
    } else {
      // nocov start
      error("Internal Error in compare_special_char_attrs; contact maintainer");
      // nocov end
    }
  }
  UNPROTECT(2);
  return res_sub;
}
// External version for unit testing

SEXP ALIKEC_compare_special_char_attrs(SEXP target, SEXP current) {
  struct VALC_settings set = VALC_settings_init();
  struct ALIKEC_res res = ALIKEC_compare_special_char_attrs_internal(
    target, current, set, 0
  );
  PROTECT(res.wrap);
  SEXP res_sexp = PROTECT(ALIKEC_res_sub_as_sxp(res, set));
  UNPROTECT(2);
  return res_sexp;
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Helper fun for `attr(dimnames(), x)`

Returns wrap object, length 2 VECSXP containing wrap call and pointer
to element to substiute
*/
SEXP ALIKEC_compare_dimnames_wrap(const char * name) {
  SEXP wrap = PROTECT(allocVector(VECSXP, 2));
  SEXP dimn_call = PROTECT(lang2(R_DimNamesSymbol, R_NilValue));
  SEXP dimn = PROTECT(mkString(name));
  SET_VECTOR_ELT(wrap, 0, lang3(ALIKEC_SYM_attr, dimn_call, dimn));
  SET_VECTOR_ELT(wrap, 1, CDDR(VECTOR_ELT(wrap, 0)));
  UNPROTECT(3);
  return(wrap);
}
/*
Compare dimnames

Code here is awful, should just return in one place, not 50...
*/

struct ALIKEC_res ALIKEC_compare_dimnames(
  SEXP prim, SEXP sec, struct VALC_settings set
) {
  struct ALIKEC_res res = ALIKEC_res_init();
  if(sec == R_NilValue) {
    res.success = 0;
    res.dat.strings.tar_pre = "have";
    res.dat.strings.target[1] = "a \"dimnames\" attribute";
    res.dat.strings.current[1] = ""; // gcc-10
    return res;
  }
  // Result will contain a SEXP, so generate a protection index for it to
  // simplify the protectin stack handling

  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(R_NilValue, &ipx);

  SEXP prim_names = PROTECT(getAttrib(prim, R_NamesSymbol));
  SEXP sec_names = PROTECT(getAttrib(sec, R_NamesSymbol));
  R_xlen_t prim_len, sec_len;
  SEXPTYPE prim_type = TYPEOF(prim), sec_type = TYPEOF(sec);

  // not a standard dimnames attribute, so do a normal alike

  if(
    prim_type != sec_type || prim_type != VECSXP ||
    (
      prim_names != R_NilValue &&
      !ALIKEC_are_special_char_attrs_internal(prim_names, sec_names)
    ) ||
    ((prim_len = XLENGTH(prim)) && prim_len != (sec_len = XLENGTH(sec)))
  ) {
    res = ALIKEC_alike_internal(prim, sec, set);
    REPROTECT(res.wrap, ipx);

    if(!res.success) {
      // Need to re-wrap the original error message
      SEXP res_call = PROTECT(lang2(R_DimNamesSymbol, R_NilValue));

      if(VECTOR_ELT(res.wrap, 1) == R_NilValue) {
        SET_VECTOR_ELT(res.wrap, 0, res_call);
      } else {
        SETCAR(VECTOR_ELT(res.wrap, 1), res_call);
      }
      SET_VECTOR_ELT(res.wrap, 1, CDR(res_call));
      UNPROTECT(1);
    }
  } else {
    /* The following likely doesn't need to be done for every dimnames so there
    probably is some optimization to be had here, should look into it if it
    seems slow; for example, `xlength` will cycle through all values, and so
    will the checking all attributes; also, do we really need to check whether
    dimnames has attributes other than names?*/

    SEXP prim_attr = ATTRIB(prim), sec_attr = ATTRIB(sec);

    /*
    Check that all `dimnames` attributes other than `names` that are both in
    target and current are alike; this is also a double loop that could be
    optimized; can't do the normal check because we need to leave out the
    `names` attribute from the comparison.  This could be simplified if we had
    an attribute comparison function that could skip a particular attribute.
    */

    SEXP prim_attr_cpy, sec_attr_cpy;
    for(
      prim_attr_cpy = prim_attr; prim_attr_cpy != R_NilValue;
      prim_attr_cpy = CDR(prim_attr_cpy)
    ) {
      SEXP prim_tag_symb = TAG(prim_attr_cpy);
      const char * prim_tag = CHAR(PRINTNAME(prim_tag_symb));
      int do_continue = 0;
      // skip names attribute
      if(prim_tag_symb == R_NamesSymbol) continue;
      for(
        sec_attr_cpy = sec_attr; sec_attr_cpy != R_NilValue;
        sec_attr_cpy = CDR(sec_attr_cpy)
      ) {
        if(prim_tag_symb == TAG(sec_attr_cpy)) {
          res = ALIKEC_alike_internal(
            CAR(prim_attr_cpy), CAR(sec_attr_cpy), set
          );
          REPROTECT(res.wrap, ipx);

          if(!res.success) {
            SEXP dimn_wrap = PROTECT(ALIKEC_compare_dimnames_wrap(prim_tag));
            SETCAR(VECTOR_ELT(res.wrap, 1), VECTOR_ELT(dimn_wrap, 0));
            SET_VECTOR_ELT(res.wrap, 1, VECTOR_ELT(dimn_wrap, 1));
            UNPROTECT(1);
            do_continue = 2;
            break;
          } else {
            do_continue = 1;
            break;
      } } }
      if(do_continue == 1) continue;    // success, next outer loop
      else if(do_continue == 2) break;  // failure, exit outer

      // missing attribute

      res.success = 0;
      res.dat.strings.tar_pre = "not be";
      res.dat.strings.target[1] = "missing";
      res.dat.strings.current[1] = ""; // gcc-10
      REPROTECT(res.wrap = ALIKEC_compare_dimnames_wrap(prim_tag), ipx);
    }
    // Compare actual dimnames attr, note that zero length primary attribute
    // matches any sec attribute

    if(res.success && prim_len) {
      // dimnames names

      if(prim_names != R_NilValue) {
        res = ALIKEC_compare_special_char_attrs_internal(
          prim_names, sec_names, set, 0
        );
        REPROTECT(res.wrap, ipx);

        if(!res.success) {
          // re-wrap in names(dimnames())
          SEXP wrap = res.wrap;
          SEXP wrap_call = PROTECT(
            lang2(R_NamesSymbol, lang2(R_DimNamesSymbol, R_NilValue))
          );
          if(VECTOR_ELT(wrap, 0) == R_NilValue) {
            // nocov start
            // All of these errors are going to have some `[x]`
            // accessor, only way would be if were using integer names, but that
            // is not actually supported here (might be with rownames); also,
            // malformed names are handled earlier in this function
            error(
              "Internal Error: %s%s",
              "it should not be possible to generate a `names(dimnames())` ",
              "error that doesn't involve some other call componenet"
            );
            // If we did hit it then the following would be how to handle it
            // SET_VECTOR_ELT(wrap, 0, wrap_call);
            // nocov end
          } else {
            SETCAR(VECTOR_ELT(wrap, 1), wrap_call);
          }
          SET_VECTOR_ELT(wrap, 1, CDR(CADR(wrap_call)));
          UNPROTECT(1);
        }
      }
      // look at dimnames themselves (i.e. not the names)

      if(res.success) {
        SEXP prim_obj, sec_obj;
        R_xlen_t attr_i;

        for(attr_i = (R_xlen_t) 0; attr_i < prim_len; attr_i++) {
          if((prim_obj = VECTOR_ELT(prim, attr_i)) != R_NilValue) {
            sec_obj = VECTOR_ELT(sec, attr_i);

            res = ALIKEC_compare_special_char_attrs_internal(
              prim_obj, sec_obj, set, 0
            );
            REPROTECT(res.wrap, ipx);
            if(!res.success) {
              SEXP wrap = res.wrap;
              SEXP wrap_call, wrap_ref;

              if(prim_len == 2) { // matrix like
                wrap_call = PROTECT(lang2(R_NilValue, R_NilValue));
                wrap_ref = CDR(wrap_call);
                PROTECT(PROTECT(R_NilValue)); // stack balance
                switch(attr_i) {
                  case (R_xlen_t) 0:
                    SETCAR(wrap_call, R_RowNamesSymbol); break;
                  case (R_xlen_t) 1:
                    SETCAR(wrap_call, ALIKEC_SYM_colnames); break;
                    // nocov start
                  default: {
                    error(
                      "Internal Error: dimnames dimension; contact maintainer."
                    );
                    // nocov end
                } }
              } else {
                SEXP dimn_call = PROTECT(lang2(R_DimNamesSymbol, R_NilValue));
                SEXP sub_ind = PROTECT(ScalarReal(attr_i + 1));
                wrap_call = PROTECT(lang3(R_Bracket2Symbol, dimn_call, sub_ind));
                wrap_ref = CDR(CADR(wrap_call));
              }
              if(VECTOR_ELT(wrap, 0) != R_NilValue) {
                SETCAR(VECTOR_ELT(wrap, 1), wrap_call);
              } else {
                SET_VECTOR_ELT(wrap, 0, wrap_call);
              }
              SET_VECTOR_ELT(wrap, 1, wrap_ref);
              UNPROTECT(3);
              break;
        } } }
    } }
  }
  // because this is an attribute error and don't want to return a recursion
  // depth recorded (we think?)

  res.dat.lvl = 0;

  // The only thing left PROTECTED should be the element we've been
  // reprotecting, and the very first two PROTECTS

  UNPROTECT(3);
  return res;
}
SEXP ALIKEC_compare_dimnames_ext(SEXP prim, SEXP sec) {
  struct VALC_settings set = VALC_settings_init();
  struct ALIKEC_res res = ALIKEC_compare_dimnames(prim, sec, set);
  PROTECT(res.wrap);
  SEXP res_sxp = PROTECT(ALIKEC_res_sub_as_sxp(res, set));
  UNPROTECT(2);
  return(res_sxp);
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Compare time series attribute; some day will have to actually get an error
display that can handle floats
*/
struct ALIKEC_res ALIKEC_compare_ts(
  SEXP target, SEXP current, struct VALC_settings set
) {
  SEXPTYPE tar_type = TYPEOF(target), cur_type = TYPEOF(current);
  struct ALIKEC_res res = ALIKEC_res_init();
  if(
    tar_type == REALSXP && cur_type == tar_type &&
    XLENGTH(target) == 3 && XLENGTH(current) == 3
  ) {
    double * tar_real = REAL(target), * cur_real = REAL(current);

    for(R_xlen_t i = 0; i < 3; i++) {
      if(tar_real[i] != 0 && tar_real[i] != cur_real[i]) {
        res.success = 0;
        char * tar_num = R_alloc(21, sizeof(char));
        char * cur_num = R_alloc(21, sizeof(char));
        snprintf(tar_num, 20, "%g", tar_real[i]);
        snprintf(cur_num, 20, "%g", cur_real[i]);

        res.dat.strings.target[1] = tar_num;
        res.dat.strings.current[1] = cur_num;
        res.wrap = PROTECT(allocVector(VECSXP, 2));
        SEXP lang_tsp = PROTECT(lang2(R_TspSymbol, R_NilValue));
        SEXP sub_idx = PROTECT(ScalarReal(i + 1));
        SEXP lang_sub =
          PROTECT(lang3(R_BracketSymbol, lang_tsp, sub_idx));
        SET_VECTOR_ELT(res.wrap, 0, lang_sub);
        SET_VECTOR_ELT(res.wrap, 1, CDR(CADR(VECTOR_ELT(res.wrap, 0))));
        UNPROTECT(4);
        return res;
    } }
  } else {
    return ALIKEC_alike_attr(target, current, "tsp", set);
  }
  return res;
}
/*
external
*/
SEXP ALIKEC_compare_ts_ext(SEXP target, SEXP current) {
  struct VALC_settings set = VALC_settings_init();
  struct ALIKEC_res res = ALIKEC_compare_ts(target, current, set);
  PROTECT(res.wrap);
  SEXP res_sxp = PROTECT(ALIKEC_res_sub_as_sxp(res, set));
  UNPROTECT(2);
  return res_sxp;
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/

struct ALIKEC_res ALIKEC_compare_levels(
  SEXP target, SEXP current, struct VALC_settings set
) {
  if(TYPEOF(target) != STRSXP || TYPEOF(current) != STRSXP)
    error("Internal Error: non-string levels; contact maintainer."); // nocov

  struct ALIKEC_res res = ALIKEC_compare_special_char_attrs_internal(
    target, current, set, 0
  );
  PROTECT(res.wrap);
  if(!res.success) {
    SEXP lang_lvl = PROTECT(lang2(R_LevelsSymbol, R_NilValue));
    ALIKEC_wrap_around(res.wrap, lang_lvl);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return res;
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
normal attribute comparison; must be alike with some exceptions for
reference attributes.

Note that we feed missing attributes as R_NilValue, which is unambiguous since
`attr(x, y) <- NULL` unsets attributes so there shouldn't be an actual attribute
with a NULL value

Since reference attrs are references to other objects that we cannot directly
compare, and could for all intents and purposes be "alike" in the typical R
sense, i.e. not pointing to exact same memory location, but otherwise the same,
we consider the fact that they are of the same type a match for alike purposes.
This is a bit of a cop out, but the situations where these attributes alone
would cause a mismatch seem pretty rare
*/

struct ALIKEC_res ALIKEC_compare_attributes_internal_simple(
  SEXP target, SEXP current, const char * attr_sym,
  struct VALC_settings set
) {
  R_xlen_t tae_val_len, cae_val_len;
  SEXPTYPE tae_type = TYPEOF(target), cae_type = TYPEOF(current);
  tae_val_len = xlength(target);
  cae_val_len = xlength(current);

  struct ALIKEC_res res = ALIKEC_res_init();

  // Start with all cases that don't produce errors

  int dont_check = !set.attr_mode && !tae_val_len;
  int both_null = tae_type == NILSXP && cae_type == NILSXP;
  int ref_obj = set.attr_mode && (
    tae_type == EXTPTRSXP || tae_type == WEAKREFSXP ||
    tae_type == BCODESXP || tae_type == ENVSXP
  );
  if(dont_check || both_null || ref_obj) return res;

  // Now checks that produce errors

  if(tae_type == NILSXP || cae_type == NILSXP) {
    res.success = 0;
    res.dat.strings.tar_pre = tae_type == NILSXP ? "not have" : "have";
    res.dat.strings.target[0] = "attribute \"%s\"";
    res.dat.strings.target[1] = attr_sym;
    res.dat.strings.cur_pre = "";
    res.dat.strings.current[0] = "";
    res.dat.strings.current[1] = ""; // gcc10 crash
    PROTECT(PROTECT(R_NilValue));
  } else if(tae_type != cae_type) {
    res.success = 0;
    res.dat.strings.target[1] = type2char(tae_type);
    res.dat.strings.current[1] = type2char(cae_type);
    SEXP attr_sym_symb = PROTECT(install(attr_sym));
    res.wrap = PROTECT(ALIKEC_attr_wrap(attr_sym_symb, R_NilValue));
  } else if (tae_val_len != cae_val_len) {
    if(set.attr_mode || tae_val_len) {
      res.success = 0;
      res.dat.strings.target[1] = CSR_len_as_chr(tae_val_len);
      res.dat.strings.current[1] = CSR_len_as_chr(cae_val_len);
      SEXP attr_sym_symb = PROTECT(install(attr_sym));
      res.wrap = PROTECT(ALIKEC_attr_wrap(attr_sym_symb, R_NilValue));
      SET_VECTOR_ELT(
        res.wrap, 0,
        lang2(ALIKEC_SYM_length, VECTOR_ELT(res.wrap, 0))
      );
    } else
      // nocov start
      error(
        "Internal Error: %s",
        "tae_val_len == 0 already checked above; contact maintainer."
      );
      // nocov end
  } else {
    res = ALIKEC_alike_attr(target, current, attr_sym, set);
    PROTECT(PROTECT(res.wrap));
  }
  UNPROTECT(2);
  return res;
}
/* Used by alike to compare attributes;

Code originally inspired by `R_compute_identical` (thanks R CORE)
*/

struct ALIKEC_res ALIKEC_compare_attributes_internal(
  SEXP target, SEXP current, struct VALC_settings set
) {
  struct ALIKEC_res res_attr = ALIKEC_res_init();

  // Note we don't protect these because target and curent should come in
  // protected so every SEXP under them should also be protected

  SEXP tar_attr, cur_attr;
  int rev = 0, is_df = 0;

  tar_attr = ATTRIB(target);
  cur_attr = ATTRIB(current);

  if(tar_attr == R_NilValue && cur_attr == R_NilValue) return res_attr;
  /*
  Array to store major errors; to see what each position corresponds to see the
  docs for ALIKEC_res.lvl

  Note there is unfortunately a protection mess here because we've transitioned
  protection schemes several times.

  We have to run through all atributes to decide what failure to show, so we're
  stuck recording at least one failure per type of failure.
  */
  struct ALIKEC_res errs[8] = {
    ALIKEC_res_init(), ALIKEC_res_init(), ALIKEC_res_init(),
    ALIKEC_res_init(), ALIKEC_res_init(), ALIKEC_res_init(),
    ALIKEC_res_init(), ALIKEC_res_init()
  };
  // store SEXPs in here for protection purposes

  SEXP errs_sexp = PROTECT(allocVector(VECSXP, 8));

  // we'll use this string to check if we're in the srcref attribute

  SEXP srcref_string = PROTECT(mkChar("srcref"));

  /*
  Mark that we're in attribute checking so we can handle recursions within
  attributes properly
  */
  set.in_attr++;

  /*
   * We sort the attribute lists so that we can more quickly compare them, but
   * also so that the failures always happen in the same order irrespective of
   * the attribute list initial order (#93)
   */

  SEXP tar_attr_sort = PROTECT(ALIKEC_list_as_sorted_vec(tar_attr));
  SEXP cur_attr_sort = PROTECT(ALIKEC_list_as_sorted_vec(cur_attr));
  SEXP tar_names = PROTECT(getAttrib(tar_attr_sort, R_NamesSymbol));
  SEXP cur_names = PROTECT(getAttrib(cur_attr_sort, R_NamesSymbol));
  if(TYPEOF(tar_names) != STRSXP && TYPEOF(cur_names) != STRSXP)
    error("Internal Error: attr data w/o names; contact maintainer."); // nocov

  R_xlen_t tar_attr_count = xlength(tar_attr_sort);
  R_xlen_t cur_attr_count = xlength(cur_attr_sort);
  R_xlen_t i, j;
  i = 0; j = 0;
  int is_names = 0;

  // A bit of a weird loop, we walk up through both sorted lists depending on
  // what attributes are missing from either list.

  while(i < tar_attr_count || j < cur_attr_count) {
    int i_implicit = 0, j_implicit = 0;
    int i_over = i >= tar_attr_count;
    int j_over = j >= cur_attr_count;
    int is_srcref = !i_over && STRING_ELT(tar_names, i) == srcref_string;

    // unfortunately complexity needs handling because we can advance i/j past
    // the end of the respective vectors

    const char * tar_tag = !i_over ? CHAR(STRING_ELT(tar_names, i)) : "";
    const char * cur_tag = !j_over ? CHAR(STRING_ELT(cur_names, j)) : "";

    if((*tar_tag == 0 && !i_over) || (*cur_tag == 0 && !j_over))
      // nocov start
      error(
        "%s%s",
        "Internal Error: encountered attribute with zero length tag. ",
        "This should not happen; contact maintainer."
      );
      // nocov end

    // Get elements to check; we use dummy NULL values if we're at the end of
    // the vectors to allow for dummy checks

    SEXP tar_attr_el_val = R_NilValue, cur_attr_el_val = R_NilValue;
    if(!i_over) tar_attr_el_val = VECTOR_ELT(tar_attr_sort, i);
    if(!j_over) cur_attr_el_val = VECTOR_ELT(cur_attr_sort, j);

    // lists are sorted by tag, so if tar_tag < cur_tag, it means that tar_tag
    // is missing from current

    int tag_cmp =
      !i_over && !j_over ? strcmp(tar_tag, cur_tag) : (i_over ? 1 : -1);

    // For class and dim, we don't want to report just that the attribute is
    // missing, we want to provide the richer error message produce by the
    // corresponding attr comparison funs.  So to allow them through, we tag
    // them as implicit.  Note we need some extra logic to make sure that the
    // attribute in question is actually missing from the other object.

    // remember attrs sorted so we will never see class - dim and dim - class
    // paired up for the same object.

    int tar_is_class = !strcmp(tar_tag, "class");
    int cur_is_class = (tar_is_class && !tag_cmp) || !strcmp(cur_tag, "class");
    int tar_is_dim = !strcmp(tar_tag, "dim");
    int cur_is_dim = (tar_is_dim && !tag_cmp) || !strcmp(cur_tag, "dim");

    if(tar_is_class + cur_is_class == 1) {
      if(tar_is_class && (tag_cmp < 0 || j_over)) j_implicit = 1;
      else if(cur_is_class && (tag_cmp > 0 || i_over) && set.attr_mode == 2)
        i_implicit = 1;
    } else if(tar_is_dim + cur_is_dim == 1) {
      if(tar_is_dim && (tag_cmp < 0 || j_over)) j_implicit = 1;
      else if(cur_is_dim && (tag_cmp > 0 || i_over) && set.attr_mode == 2)
        i_implicit = 1;
    }
    if(i_implicit || j_implicit) {
      if(i_implicit) {
        tar_tag = cur_tag;
        tar_attr_el_val = R_NilValue;
      } else {
        cur_tag = tar_tag;
        cur_attr_el_val = R_NilValue;
      }
      tag_cmp = 0;  // pretend attr available in tar and cur
    }
    // Handle attribute missing from one or the other

    if(tag_cmp < 0) {
      // cur is missing something missing tar has; we care unless it is src_ref
      // and in default mode

      if(errs[7].success && (set.attr_mode || !is_srcref)) {
        errs[7].success = 0;
        errs[7].dat.strings.tar_pre = "have";
        errs[7].dat.strings.target[0] = "attribute \"%s\"";
        errs[7].dat.strings.target[1] = tar_tag;
        errs[7].dat.strings.cur_pre = "";  // need to blank this
        errs[7].dat.strings.current[0] = "";  // need to blank this
        errs[7].dat.strings.current[1] = "";  // gcc10
      }
      ++i;
    } else if(tag_cmp > 0) {
      // tar has something missing from cur, only matters if in mode == 2

      if(errs[7].success && set.attr_mode == 2) {
        errs[7].success = 0;
        errs[7].dat.strings.tar_pre = "not have";
        errs[7].dat.strings.target[0] = "attribute \"%s\"";
        errs[7].dat.strings.target[1] = CHAR(STRING_ELT(cur_names, j));
        errs[7].dat.strings.cur_pre = "";  // need to blank this
        errs[7].dat.strings.current[0] = "";  // need to blank this
        errs[7].dat.strings.current[1] = "";  // gcc10
      }
      ++j;
    }
    // At this point, if there was a tag difference we want to loop to next
    // value or break.

    if(tag_cmp) {if(i_over || j_over) break; else continue;};

    // we have matching tag values, so now compare the attribute values and also
    // advance counters if appropriate.

    if(!i_over && !i_implicit) ++i;
    if(!j_over && !j_implicit) ++j;

    // = Baseline Check ========================================================

    if(set.attr_mode && errs[6].success) {
      // this contains returns a SEXP
      if(tar_attr_el_val != R_NilValue || set.attr_mode == 2 ) {
        errs[6] = ALIKEC_compare_attributes_internal_simple(
          tar_attr_el_val, cur_attr_el_val, tar_tag, set
        );
        SET_VECTOR_ELT(errs_sexp, 6, errs[6].wrap);
      }
    // = Custom Checks =========================================================

    /* see alike documentation for explanations of how the special
    attributes in this section are compared */

    } else {
      // - Class ---------------------------------------------------------------

      /* Class errors always trump all others so no need to calculate further;
      for every other error we have to keep going in case we eventually find a
      class error*/

      if(tar_is_class) {
        SEXP cur_attr_el_val_tmp =
          PROTECT(ALIKEC_class(current, cur_attr_el_val));
        SEXP tar_attr_el_val_tmp =
          PROTECT(ALIKEC_class(target, tar_attr_el_val));
        struct ALIKEC_res class_comp = ALIKEC_compare_class(
          tar_attr_el_val_tmp, cur_attr_el_val_tmp, set
        );
        UNPROTECT(2);
        is_df = class_comp.dat.df;
        errs[0] = class_comp;
        SET_VECTOR_ELT(errs_sexp, 0, errs[6].wrap);
        if(!class_comp.success) break;

      // - Names ---------------------------------------------------------------

      } else if (
        (is_names = !strcmp(tar_tag, "names")) || !strcmp(tar_tag, "row.names")
      ) {
        int err_ind = is_names ? 3 : 4;
        struct ALIKEC_res name_comp =
          ALIKEC_compare_special_char_attrs_internal(
            tar_attr_el_val, cur_attr_el_val, set, 0
          );
        SET_VECTOR_ELT(errs_sexp, err_ind, name_comp.wrap);
        if(!name_comp.success) {
          errs[err_ind] = name_comp;

          // wrap original wrap in names/rownames

          SEXP wrap_orig = errs[err_ind].wrap;
          SEXP call = PROTECT(
            lang2(is_names ? R_NamesSymbol : R_RowNamesSymbol, R_NilValue)
          );
          ALIKEC_wrap_around(wrap_orig, call); // modifies wrap_orig
          UNPROTECT(1);
        }
        continue;
      // - Dims ----------------------------------------------------------------

      } else if (tar_is_dim && set.attr_mode == 0) {
        int err_ind = 2;
        struct ALIKEC_res dim_comp = ALIKEC_compare_dims(
          tar_attr_el_val, cur_attr_el_val, target, current, set
        );
        if(dim_comp.dat.lvl) err_ind = 0;
        SET_VECTOR_ELT(errs_sexp, err_ind, dim_comp.wrap);

        // implicit class error upgrades to major error

        errs[err_ind] = dim_comp;

      // - dimnames ------------------------------------------------------------

      } else if (!strcmp(tar_tag, "dimnames")) {
        struct ALIKEC_res dimname_comp = ALIKEC_compare_dimnames(
          tar_attr_el_val, cur_attr_el_val, set
        );
        SET_VECTOR_ELT(errs_sexp, 5, dimname_comp.wrap);
        errs[5] = dimname_comp;

      // - levels --------------------------------------------------------------

      } else if (!strcmp(tar_tag, "levels")) {
        struct ALIKEC_res levels_comp = ALIKEC_compare_levels(
          tar_attr_el_val, cur_attr_el_val, set
        );
        SET_VECTOR_ELT(errs_sexp, 6, levels_comp.wrap);
        errs[6] = levels_comp;

      // - tsp -----------------------------------------------------------------

      } else if (!strcmp(tar_tag, "tsp")) {

        struct ALIKEC_res ts_comp = ALIKEC_compare_ts(
          tar_attr_el_val, cur_attr_el_val, set
        );
        SET_VECTOR_ELT(errs_sexp, 1, ts_comp.wrap);
        errs[1] = ts_comp;

      // - normal attrs --------------------------------------------------------

      } else {
        struct ALIKEC_res attr_comp =
          ALIKEC_compare_attributes_internal_simple(
            tar_attr_el_val, cur_attr_el_val, tar_tag, set
          );
        SET_VECTOR_ELT(errs_sexp, 6, attr_comp.wrap);
        errs[6] = attr_comp;
      }
  } }
  // Now determine which error to throw, if any

  if(!set.in_attr) {
    // nocov start
    error(
      "Internal Error: attribute depth counter corrupted; contact maintainer"
    );
    // nocov end
  }
  set.in_attr--;

  for(int i = 0; i < 8; i++) {
    if(!errs[i].success && (!rev || (rev && set.attr_mode == 2))) {
      res_attr = errs[i];
      res_attr.dat.lvl = i;
      break;
  } }
  res_attr.dat.df = is_df;
  UNPROTECT(6);
  return res_attr;
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
external interface for compare attributes
*/
SEXP ALIKEC_compare_attributes(SEXP target, SEXP current, SEXP attr_mode) {
  SEXPTYPE attr_mode_type = TYPEOF(attr_mode);

  if(
    (attr_mode_type != INTSXP && attr_mode_type != REALSXP) ||
    XLENGTH(attr_mode) != 1
  )
    error("Argument `mode` must be a one length integer like vector");

  struct VALC_settings set = VALC_settings_init();
  set.attr_mode = asInteger(attr_mode);

  struct ALIKEC_res comp_res =
    ALIKEC_compare_attributes_internal(target, current, set);
  PROTECT(comp_res.wrap);
  SEXP res = ALIKEC_res_sub_as_sxp(comp_res, set);
  UNPROTECT(1);
  return res;
}
