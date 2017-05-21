#include "alike.h"

SEXP ALIKEC_res_sub_as_sxp(struct ALIKEC_res_sub sub) {
  PROTECT(sub.message);
  SEXP out = PROTECT(allocVector(VECSXP, 4));
  SEXP out_names = PROTECT(allocVector(STRSXP, 4));
  const char * names[4] = {"success", "message", "df", "lvl"};
  int i;

  for(i = 0; i < 4; i++) SET_STRING_ELT(out_names, i, mkChar(names[i]));

  SET_VECTOR_ELT(out, 0, ScalarInteger(sub.success));
  SET_VECTOR_ELT(out, 1, sub.message);
  SET_VECTOR_ELT(out, 2, ScalarInteger(sub.df));
  SET_VECTOR_ELT(out, 3, ScalarInteger(sub.lvl));
  setAttrib(out, R_NamesSymbol, out_names);
  UNPROTECT(3);

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
    SET_VECTOR_ELT(wrap, 0, lang3(ALIKEC_SYM_attr, call, tag_name));
    UNPROTECT(1);
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
- `attr_attr` indicates we are checking the attributes of an attribute; NOTE:
  can currently no longer remember how/why this should be used, we used to
  use `attr` when this was TRUE, and `attributes` when not, but that doesn't
  make much sense?
*/

struct ALIKEC_res_sub ALIKEC_alike_attr(
  SEXP target, SEXP current, SEXP attr_symb,
  struct ALIKEC_settings set, int attr_attr
) {
  struct ALIKEC_res res = ALIKEC_alike_internal(target, current, set);
  struct ALIKEC_res_sub res_sub = ALIKEC_res_sub_def();

  if(!res.success) {
    res_sub.success = 0;
    res_sub.message = PROTECT(
      ALIKEC_res_msg_def(
        "be", "`alike` the corresponding element in target", "", ""
      )
    );
    // if(attr_attr) {
    //   wrap_call = PROTECT(
    //     lang3(ALIKEC_SYM_attr, R_NilValue, mkString(attr_name))
    //   );
    // } else {
    //   wrap_call = PROTECT(lang2(R_NilValue, R_NilValue));
    //   SET_CAR(
    //     wrap_call,
    //     special ? install(attr_name) : ALIKEC_SYM_attributes
    //   );
    // }
    SEXP wrap = PROTECT(ALIKEC_attr_wrap(attr_symb, R_NilValue));
    SET_VECTOR_ELT(res_sub.message, 1, wrap);
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
struct ALIKEC_res_sub ALIKEC_compare_class(
  SEXP target, SEXP current, struct ALIKEC_settings set
) {
  if(TYPEOF(current) != STRSXP || TYPEOF(target) != STRSXP)
    return ALIKEC_alike_attr(target, current, R_ClassSymbol, set, 0);

  int tar_class_len, cur_class_len, len_delta, tar_class_i, cur_class_i,
      is_df = 0;
  const char * cur_class;
  const char * tar_class;
  struct ALIKEC_res_sub res = ALIKEC_res_sub_def();
  res.message = PROTECT(ALIKEC_res_msg_def("", "", "", ""));

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

    if(res.success && strcmp(cur_class, tar_class)) { // class mismatch
      res.success = 0;

      SEXP msg_strings = VECTOR_ELT(res.message, 0);

      if(cur_class_len > 1) {
        SEXP wrap_call = PROTECT(
          lang3(
            R_BracketSymbol, lang2(R_ClassSymbol, R_NilValue),
            ScalarReal(cur_class_i + 1)
        ) );
        SEXP wrap = PROTECT(allocVector(VECSXP, 2));

        SET_VECTOR_ELT(wrap, 0, wrap_call);
        SET_VECTOR_ELT(wrap, 1, CDR(CADR(wrap_call)));
        SET_VECTOR_ELT(res.message, 1, wrap);
        UNPROTECT(2);

        SET_STRING_ELT(msg_strings, 0, mkChar("be"));
        SET_STRING_ELT(
          msg_strings, 1, mkChar(
            CSR_smprintf4(ALIKEC_MAX_CHAR, "\"%s\"", tar_class, "", "", "")
        ) );
        SET_STRING_ELT(msg_strings, 2, mkChar("is"));
        SET_STRING_ELT(
          msg_strings, 3,
          mkChar(
            CSR_smprintf4(ALIKEC_MAX_CHAR, "\"%s\"", cur_class, "", "", "")
        ) );
      } else {
        SET_STRING_ELT(msg_strings, 0, mkChar("be"));
        SET_STRING_ELT(
            msg_strings, 1,
            mkChar(
              CSR_smprintf4(
                ALIKEC_MAX_CHAR,  "class \"%s\"", tar_class, "", "", ""
        ) ) );
        SET_STRING_ELT(msg_strings, 2, mkChar("is"));
        SET_STRING_ELT(
          msg_strings, 3,
            mkChar(
              CSR_smprintf4(
                ALIKEC_MAX_CHAR,  "\"%s\"", cur_class, "", "", ""
        ) ) );
  } } }
  // Check to make sure have enough classes

  if(res.success) {
    if(tar_class_len > cur_class_len) {
      res.success = 0;
      SET_STRING_ELT(VECTOR_ELT(res.message, 0),  0, mkChar("inherit"));
      SET_STRING_ELT(
        VECTOR_ELT(res.message, 0),  1,
        mkChar(
          CSR_smprintf4(
            ALIKEC_MAX_CHAR, "from class \"%s\"",
            CHAR(STRING_ELT(target, tar_class_i)), "", "", ""
  ) ) );} }
  // Make sure class attributes are alike

  if(res.success) {
    UNPROTECT(1);
    res = ALIKEC_alike_attr(
      ATTRIB(target), ATTRIB(current), R_ClassSymbol, set, 1
    );
    PROTECT(res.message);
  }
  UNPROTECT(1);
  res.df = is_df;
  return res;
}
SEXP ALIKEC_compare_class_ext(SEXP target, SEXP current) {
  struct ALIKEC_res_sub res = ALIKEC_compare_class(
    target, current, ALIKEC_set_def("")
  );
  PROTECT(res.message);
  SEXP res_sxp = ALIKEC_res_sub_as_sxp(res);
  UNPROTECT(1);
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
struct ALIKEC_res_sub ALIKEC_compare_dims(
  SEXP target, SEXP current, SEXP tar_obj, SEXP cur_obj,
  struct ALIKEC_settings set
) {
  // Invalid dims

  if(
    (TYPEOF(target) != INTSXP && target != R_NilValue) ||
    (TYPEOF(current) != INTSXP && current != R_NilValue)
  )
    return ALIKEC_alike_attr(target, current, R_DimSymbol, set, 0);

  // Dims -> implicit class

  R_xlen_t target_len = xlength(target), target_len_cap;
  target_len_cap = target_len > (R_xlen_t) 3 ? (R_xlen_t) 3 : target_len;
  R_xlen_t current_len = xlength(current), current_len_cap;
  current_len_cap = current_len > (R_xlen_t) 3 ? (R_xlen_t) 3 : current_len;

  struct ALIKEC_res_sub res = ALIKEC_res_sub_def();
  const char * class_err_target = "";
  const char * class_err_actual = "";

  if(target_len_cap > 1 && isVectorAtomic(tar_obj)) {
    if(current == R_NilValue) {  // current should be matrix/array
      class_err_target = target_len_cap > 2 ? "array" : "matrix";
      class_err_actual = CHAR(asChar(ALIKEC_mode(cur_obj)));
    } else if(isVectorAtomic(cur_obj) && current_len_cap != target_len_cap) {
      // target is matrix/array
      class_err_target = target_len_cap > 2 ? "array" : "matrix";
      class_err_actual = current_len_cap == 2 ?
        "matrix" : (current_len_cap == 1 ? "vector" : "array");
    } else if(!isVectorAtomic(cur_obj)) {
      // In this case, target is atomic, but current is not, would normally be
      // caught by earlier type comparisons so shouldn't get here unless testing
      // explicitly
      class_err_target = CHAR(asChar(ALIKEC_mode(tar_obj)));
      class_err_actual = type2char(TYPEOF(cur_obj));
    }
  } else if (current_len_cap > 1 && isVectorAtomic(cur_obj)) {
    if(isVectorAtomic(tar_obj)) {
      class_err_target = CHAR(asChar(ALIKEC_mode(tar_obj)));
      class_err_actual = current_len_cap == 2 ? "matrix" : "array";
    } else {
      class_err_target = CHAR(asChar(ALIKEC_mode(tar_obj)));
      class_err_actual = CHAR(asChar(ALIKEC_mode(cur_obj)));
    }
  }
  if(class_err_target[0]) {
    res.success = 0;
    res.lvl = 1;
    res.message = ALIKEC_res_msg_def(
      "be",
      CSR_smprintf4(ALIKEC_MAX_CHAR, "\"%s\"", class_err_target, "", "", ""),
      "is",
      CSR_smprintf4(ALIKEC_MAX_CHAR, "\"%s\"", class_err_actual, "", "", "")
    );
    return res;
  }
  // Normal dim checking

  if(current == R_NilValue) {
    res.success = 0;
    res.message = ALIKEC_res_msg_def("have", "a \"dim\" attribute", "", "");
    return res;
  }
  if(target_len != current_len) {
    res.success = 0;
    res.message = ALIKEC_res_msg_def(
      "have",
      CSR_smprintf4(
        ALIKEC_MAX_CHAR, "%s dimension%s",
        CSR_len_as_chr(target_len), target_len == (R_xlen_t) 1 ? "" : "s",
        "", ""
      ),
      "has",
      CSR_smprintf4(
        ALIKEC_MAX_CHAR, "%s", CSR_len_as_chr(current_len), "", "", ""
    ));
    return res;
  }
  R_xlen_t attr_i;
  int tar_dim_val;

  for(attr_i = (R_xlen_t)0; attr_i < target_len; attr_i++) {
    tar_dim_val = INTEGER(target)[attr_i];
    const char * tar_dim_chr = CSR_len_as_chr((R_xlen_t)tar_dim_val);
    char * err_dim1, * err_dim2;

    if(tar_dim_val && tar_dim_val != INTEGER(current)[attr_i]) {
      if(target_len == 2) {  // Matrix
        err_dim1 = "";
        const char * err_dimtmp;
        switch(attr_i) {
          case (R_xlen_t) 0: err_dimtmp = "row%s"; break;
          case (R_xlen_t) 1: err_dimtmp = "column%s"; break;
          default:
            // nocov start
            error(
              "%s%s",
              "Internal Error: inconsistent matrix dimensions; contact  ",
              "maintainer."
            );
            // nocov end
        }
        err_dim2 = (char *) CSR_smprintf4(
          ALIKEC_MAX_CHAR, err_dimtmp, tar_dim_val == 1 ? "" : "s", "", "", ""
        );
      } else {
        err_dim1 = "size ";
        err_dim2 = (char *) CSR_smprintf4(
          ALIKEC_MAX_CHAR, "at dimension %s",
          CSR_len_as_chr((R_xlen_t)(attr_i + 1)), "", "", ""
      );}
      res.success = 0;
      res.message = ALIKEC_res_msg_def(
        "have",
        CSR_smprintf4(
          ALIKEC_MAX_CHAR, "%s%s %s",
          (const char *) err_dim1, tar_dim_chr, (const char *) err_dim2, ""
        ),
        "has",
        CSR_smprintf4(
          ALIKEC_MAX_CHAR, "%s",
          CSR_len_as_chr((R_xlen_t)(INTEGER(current)[attr_i])), "", "", ""
      ));
      return res;
  } }
  return ALIKEC_alike_attr(target, current, R_DimSymbol, set, 1);
}
SEXP ALIKEC_compare_dim_ext(
  SEXP target, SEXP current, SEXP tar_obj, SEXP cur_obj
) {
  return ALIKEC_res_sub_as_sxp(
    ALIKEC_compare_dims(
      target, current, tar_obj, cur_obj, ALIKEC_set_def("")
  ) );
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
string containing `%s` that can then be used to sprintf in the name of the
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
struct ALIKEC_res_sub ALIKEC_compare_special_char_attrs_internal(
  SEXP target, SEXP current, struct ALIKEC_settings set, int strict
) {
  // We're playing with fire a little with PROTECT since we're not actually
  // PROTECTing the result of ALIKEC_res_msg_def in most cases to avoid
  // having to keep the stack balance across all branches; in theory the code
  // returns before there should be any gc happening

  struct ALIKEC_res res = ALIKEC_alike_internal(target, current, set);
  PROTECT(res.message);
  struct ALIKEC_res_sub res_sub = ALIKEC_res_sub_def();

  // Special character attributes must be alike; not sure the logic here is
  // completely correct, will have to verify

  if(!res.success) {
    res_sub.success = 0;
    res_sub.message = res.message;
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
        res_sub.message = ALIKEC_res_msg_def(
          "be", "identical to target", "", ""
        );
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
            res_sub.success=0;
            res_sub.message = PROTECT(
              ALIKEC_res_msg_def(
                "be",
                CSR_smprintf4(
                  ALIKEC_MAX_CHAR, "\"%s\"", tar_name_val, "", "", ""
                ),
                "is",
                CSR_smprintf4(
                    ALIKEC_MAX_CHAR, "\"%s\"", cur_name_val, "", "", ""
            ) ) );
            SEXP wrap = PROTECT(allocVector(VECSXP, 2));
            SET_VECTOR_ELT(wrap, 0,
              lang3(R_BracketSymbol, R_NilValue, ScalarReal(i + 1))
            );
            SET_VECTOR_ELT(wrap, 1, CDR(VECTOR_ELT(wrap, 0)));
            SET_VECTOR_ELT(res_sub.message, 1, wrap);
            UNPROTECT(2);
            break;
      } } }
    } else {
      // nocov start
      error("Internal Error in compare_special_char_attrs; contact maintainer");
      // nocov end
    }
  }
  UNPROTECT(1);
  return res_sub;
}
// External version for unit testing

SEXP ALIKEC_compare_special_char_attrs(SEXP target, SEXP current) {
  struct ALIKEC_res_sub res = ALIKEC_compare_special_char_attrs_internal(
    target, current, ALIKEC_set_def(""), 0
  );
  PROTECT(res.message);
  SEXP res_sexp = PROTECT(ALIKEC_res_sub_as_sxp(res));
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
  SET_VECTOR_ELT(
    wrap, 0, lang3(
      ALIKEC_SYM_attr, lang2(R_DimNamesSymbol, R_NilValue),
      mkString(name)
  ) );
  SET_VECTOR_ELT(wrap, 1, CDDR(VECTOR_ELT(wrap, 0)));
  UNPROTECT(1);
  return(wrap);
}
/*
Compare dimnames
*/

struct ALIKEC_res_sub ALIKEC_compare_dimnames(
  SEXP prim, SEXP sec, struct ALIKEC_settings set
) {
  struct ALIKEC_res_sub res = ALIKEC_res_sub_def();
  if(sec == R_NilValue) {
    res.success = 0;
    res.message =
      ALIKEC_res_msg_def("have", "a \"dimnames\" attribute", "", "");
    return res;
  }
  SEXP prim_names = getAttrib(prim, R_NamesSymbol);
  SEXP sec_names = getAttrib(sec, R_NamesSymbol);
  R_xlen_t prim_len, sec_len;
  SEXPTYPE prim_type = TYPEOF(prim);
  if( // not a standard dimnames attribute
    prim_type != TYPEOF(sec) || prim_type != VECSXP ||
    (
      prim_names != R_NilValue &&
      !ALIKEC_are_special_char_attrs_internal(prim_names, sec_names)
    ) ||
    ((prim_len = XLENGTH(prim)) && prim_len != (sec_len = XLENGTH(sec)))
  ) {
    struct ALIKEC_res res_tmp = ALIKEC_alike_internal(prim, sec, set);
    PROTECT(res_tmp.message);
    if(!res_tmp.success) {
      // Need to re-wrap the original error message
      res.success = 0;
      res.message = res_tmp.message;
      SEXP res_wrap_old = VECTOR_ELT(res.message, 1);
      SEXP res_call = PROTECT(lang2(R_DimNamesSymbol, R_NilValue));

      if(VECTOR_ELT(res_wrap_old, 1) == R_NilValue) {
        SET_VECTOR_ELT(res_wrap_old, 0, res_call);
      } else {
        SETCAR(VECTOR_ELT(res_wrap_old, 1), res_call);
      }
      SET_VECTOR_ELT(res_wrap_old, 1, CDR(res_call));
      UNPROTECT(1);
    }
    UNPROTECT(1);
    return res;
  }
  /* The following likely doesn't need to be done for every dimnames so there
  probably is some optimization to be had here, should look into it if it
  seems slow; for example, `xlength` will cycle through all values, and so will
  the checking all attributes; also, do we really need to check whether dimnames
  has attributes other than names?*/

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
    if(prim_tag_symb == R_NamesSymbol) continue;
    for(
      sec_attr_cpy = sec_attr; sec_attr_cpy != R_NilValue;
      sec_attr_cpy = CDR(sec_attr_cpy)
    ) {
      if(prim_tag_symb == TAG(sec_attr_cpy)) {
        struct ALIKEC_res res_tmp = ALIKEC_alike_internal(
          CAR(prim_attr_cpy), CAR(sec_attr_cpy), set
        );
        if(!res_tmp.success) {
          PROTECT(res_tmp.message); // not really necessary since unused
          res.success = 0;
          res.message = PROTECT(
            ALIKEC_res_msg_def(
              "be",  "`alike` the corresponding element in target", "", ""
          ) );
          SEXP wrap = PROTECT(ALIKEC_compare_dimnames_wrap(prim_tag));
          SET_VECTOR_ELT(res.message, 1, wrap);
          UNPROTECT(3);
          return res;
        }
        do_continue = 1;
        res_tmp.message = R_NilValue;
        break;
    } }
    if(do_continue) continue;
    // missing attribute
    res.success = 0;
    res.message = PROTECT(ALIKEC_res_msg_def("not be", "missing", "", ""));
    SEXP wrap = PROTECT(ALIKEC_compare_dimnames_wrap(prim_tag));
    SET_VECTOR_ELT(res.message, 1, wrap);
    UNPROTECT(2);
    return res;
  }
  // Compare actual dimnames attr

  if(!prim_len) return res;  // zero length list matches anything

  // dimnames names

  if(prim_names != R_NilValue) {
    struct ALIKEC_res_sub dimnames_name_comp =
      ALIKEC_compare_special_char_attrs_internal(prim_names, sec_names, set, 0);
    if(!dimnames_name_comp.success) {
      PROTECT(dimnames_name_comp.message);
      // re-wrap in names(dimnames())
      SEXP wrap = VECTOR_ELT(dimnames_name_comp.message, 1);
      SEXP wrap_call = PROTECT(
        lang2(R_NamesSymbol, lang2(R_DimNamesSymbol, R_NilValue))
      );
      if(VECTOR_ELT(wrap, 0) == R_NilValue) {
        // nocov start
        // All of these errors are going to have some `[x]` accessor, only way
        // would be if were using integer names, but that is not actually
        // supported here (might be with rownames); also, malformed names are
        // handled earlier in this function
        error(
          "Internal Error: %s%s",
          "it should not be possible to generate a `names(dimnames())` error ",
          "that doesn't involve some other call componenet"
        );
        // If we did hit this case, then the following would be how to handle it
        // SET_VECTOR_ELT(wrap, 0, wrap_call);
        // nocov end
      } else {
        SETCAR(VECTOR_ELT(wrap, 1), wrap_call);
      }
      SET_VECTOR_ELT(wrap, 1, CDR(CADR(wrap_call)));
      UNPROTECT(2);
      return dimnames_name_comp;
    }
    dimnames_name_comp.message = R_NilValue;
  }
  // look at dimnames themselves

  SEXP prim_obj, sec_obj;
  R_xlen_t attr_i;

  for(attr_i = (R_xlen_t) 0; attr_i < prim_len; attr_i++) {
    if((prim_obj = VECTOR_ELT(prim, attr_i)) != R_NilValue) {
      sec_obj = VECTOR_ELT(sec, attr_i);

      struct ALIKEC_res_sub dimnames_comp =
        ALIKEC_compare_special_char_attrs_internal(
          prim_obj, sec_obj, set, 0
        );
      if(!dimnames_comp.success) {
        PROTECT(dimnames_comp.message);
        SEXP wrap = VECTOR_ELT(dimnames_comp.message, 1);
        SEXP wrap_call, wrap_ref;

        if(prim_len == 2) { // matrix like
          wrap_call = PROTECT(lang2(R_NilValue, R_NilValue));
          wrap_ref = CDR(wrap_call);
          switch(attr_i) {
            case (R_xlen_t) 0: SETCAR(wrap_call, R_RowNamesSymbol); break;
            case (R_xlen_t) 1: SETCAR(wrap_call, ALIKEC_SYM_colnames); break;
            default: {
              // nocov start
              error("Internal Error: dimnames dimension; contact maintainer.");
              // nocov end
            }
          }
        } else {
          wrap_call = PROTECT(
            lang3(
              R_Bracket2Symbol, lang2(R_DimNamesSymbol, R_NilValue),
              ScalarReal(attr_i + 1)
          ) );
          wrap_ref = CDR(CADR(wrap_call));
        }
        if(VECTOR_ELT(wrap, 0) != R_NilValue) {
          SETCAR(VECTOR_ELT(wrap, 1), wrap_call);
        } else {
          SET_VECTOR_ELT(wrap, 0, wrap_call);
        }
        SET_VECTOR_ELT(wrap, 1, wrap_ref);
        UNPROTECT(2);
        return dimnames_comp;
  } } }
  return res;
}
SEXP ALIKEC_compare_dimnames_ext(SEXP prim, SEXP sec) {
  return(
    ALIKEC_res_sub_as_sxp(
      ALIKEC_compare_dimnames(prim, sec, ALIKEC_set_def(""))
  ) );
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/
/*
Compare time series attribute; some day will have to actually get an error
display that can handle floats
*/
struct ALIKEC_res_sub ALIKEC_compare_ts(
  SEXP target, SEXP current, struct ALIKEC_settings set
) {
  SEXPTYPE tar_type = TYPEOF(target);
  struct ALIKEC_res_sub res = ALIKEC_res_sub_def();
  if(
    tar_type == REALSXP && TYPEOF(current) == tar_type &&
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
        res.message = PROTECT(
          ALIKEC_res_msg_def(
            "be",
            CSR_smprintf4(
              ALIKEC_MAX_CHAR, "%s", tar_num, "", "", ""
            ),
            "is",
            CSR_smprintf4(
              ALIKEC_MAX_CHAR, "%s", cur_num, "", "", ""
            )
        ) );
        SEXP wrap = PROTECT(allocVector(VECSXP, 2));
        SET_VECTOR_ELT(
          wrap, 0, lang3(
            R_BracketSymbol, lang2(R_TspSymbol, R_NilValue),
            ScalarReal(i + 1)
        ) );
        SET_VECTOR_ELT(wrap, 1, CDR(CADR(VECTOR_ELT(wrap, 0))));
        SET_VECTOR_ELT(res.message, 1, wrap);
        UNPROTECT(2);
        return res;
    } }
  } else {
    return ALIKEC_alike_attr(target, current, R_TspSymbol, set, 0);
  }
  return res;
}
/*
external
*/
SEXP ALIKEC_compare_ts_ext(SEXP target, SEXP current) {
  return ALIKEC_res_sub_as_sxp(
    ALIKEC_compare_ts(target, current, ALIKEC_set_def())
  );
}
/*-----------------------------------------------------------------------------\
\-----------------------------------------------------------------------------*/

struct ALIKEC_res_sub  ALIKEC_compare_levels(
  SEXP target, SEXP current, struct ALIKEC_settings set
) {
  if(TYPEOF(target) == STRSXP && TYPEOF(current) == STRSXP) {
    struct ALIKEC_res_sub res = ALIKEC_compare_special_char_attrs_internal(
      target, current, set, 0
    );
    PROTECT(res.message);
    if(!res.success) {
      ALIKEC_wrap_around(
        VECTOR_ELT(res.message, 1), lang2(R_LevelsSymbol, R_NilValue)
      );
    }
    UNPROTECT(1);
    return res;
  }
  return ALIKEC_alike_attr(target, current, R_LevelsSymbol, set, 0);
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

struct ALIKEC_res_sub ALIKEC_compare_attributes_internal_simple(
  SEXP target, SEXP current, SEXP attr_sym,
  struct ALIKEC_settings set
) {
  R_xlen_t tae_val_len, cae_val_len;
  SEXPTYPE tae_type = TYPEOF(target), cae_type = TYPEOF(current);
  tae_val_len = xlength(target);
  cae_val_len = xlength(current);

  struct ALIKEC_res_sub res = ALIKEC_res_sub_def();

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
    res.message = PROTECT(
      ALIKEC_res_msg_def(
        CSR_smprintf4(
          ALIKEC_MAX_CHAR, "%shave",
          tae_type == NILSXP ? "not " : "", "", "", ""
        ),
        CSR_smprintf4(
          ALIKEC_MAX_CHAR, "attribute \"%s\"",
          CHAR(PRINTNAME(attr_sym)), "", "", ""
        ),
        "", ""
    ) );
  } else if(tae_type != cae_type) {
    res.success = 0;
    res.message = PROTECT(
      ALIKEC_res_msg_def(
        "be",
        CSR_smprintf4(
          ALIKEC_MAX_CHAR, "%s", type2char(tae_type), "", "", ""
        ),
        "is",
        CSR_smprintf4(
          ALIKEC_MAX_CHAR, "%s", type2char(cae_type), "", "", ""
        )
    ) );
    SEXP wrap = PROTECT(ALIKEC_attr_wrap(attr_sym, R_NilValue));
    SET_VECTOR_ELT(res.message, 1, wrap);
    UNPROTECT(1);
  } else if (tae_val_len != cae_val_len) {
    if(set.attr_mode || tae_val_len) {
      res.success = 0;
      res.message = PROTECT(
        ALIKEC_res_msg_def(
          "be",
          CSR_smprintf4(
            ALIKEC_MAX_CHAR, "%s", CSR_len_as_chr(tae_val_len), "", "", ""
          ),
          "is",
          CSR_smprintf4(
            ALIKEC_MAX_CHAR, "%s", CSR_len_as_chr(cae_val_len), "", "", ""
          )
      ) );
      SEXP wrap = PROTECT(ALIKEC_attr_wrap(attr_sym, R_NilValue));
      SET_VECTOR_ELT(
        wrap, 0,
        lang2(ALIKEC_SYM_length, VECTOR_ELT(wrap, 0))
      );
      SET_VECTOR_ELT(res.message, 1, wrap);
      UNPROTECT(1);
    }
  } else {
    res = ALIKEC_alike_attr(target, current, attr_sym, set, 0);
    PROTECT(res.message);
  }
  UNPROTECT(1);
  return res;
}
/* Used by alike to compare attributes;

Code originally inspired by `R_compute_identical` (thanks R CORE)
*/

struct ALIKEC_res_sub ALIKEC_compare_attributes_internal(
  SEXP target, SEXP current, struct ALIKEC_settings set
) {
  struct ALIKEC_res_sub res_attr = ALIKEC_res_sub_def(), res_sub;

  // Note we don't protect these because target and curent should come in
  // protected so every SEXP under them should also be protected

  SEXP tar_attr, cur_attr, prim_attr, sec_attr;
  int rev = 0, is_df = 0;

  tar_attr = ATTRIB(target);
  cur_attr = ATTRIB(current);


  if(tar_attr == R_NilValue && cur_attr == R_NilValue) return res_attr;

  /*
  Array to store major errors from, in order:
    0. class,
    1. tsp
    2. dim
    3. names
    4. rownames
    5. dimnames
    6. other
    7. missing
  Note there is unfortunately a protection mess here because the intermediate
  functions return a struct containing SEXPs, so we need to PROTECT them
  and there is a variable number of protections since we can't stop on
  first failure.  Also don't want to switch to pure SEXP since that is not
  at all transparent in terms of knowing what element is being accessed*/

  struct ALIKEC_res_sub errs[8] = {
    ALIKEC_res_sub_def(), ALIKEC_res_sub_def(), ALIKEC_res_sub_def(),
    ALIKEC_res_sub_def(), ALIKEC_res_sub_def(), ALIKEC_res_sub_def(),
    ALIKEC_res_sub_def(), ALIKEC_res_sub_def()
  };
  size_t ps = 0;  // track protect stack size

  if(tar_attr == R_NilValue) {
    rev = 1;
    prim_attr = cur_attr;
    sec_attr = tar_attr;
    if(set.attr_mode == 2) {
      errs[7].success = 0;
      errs[7].message =
        PROTECT(ALIKEC_res_msg_def("have", "attributes", "", ""));
    } else PROTECT(R_NilValue);
  } else {
    prim_attr = tar_attr;
    sec_attr = cur_attr;
    if(
      cur_attr == R_NilValue &&
      (
       set.attr_mode ||
       // if the only attribute in `tar_attr` is srcref then it is okay that
       // `cur_attr` is NULL
       !(
         !strcmp(CHAR(PRINTNAME(TAG(prim_attr))), "srcref") &&
         CDR(prim_attr) == R_NilValue
      ) )
    ) {
      errs[7].success = 0;
      errs[7].message = PROTECT(
        ALIKEC_res_msg_def(
          "have",
          "attributes",
          "has",
          "none"
      ) );
    } else PROTECT(R_NilValue);
  }
  ps++;
  /*
  Mark that we're in attribute checking so we can handle recursions within
  attributes properly
  */
  set.in_attr++;

  /*
  Loop through all attr combinations; maybe could be made faster by reducing the
  second loop each time a match is found, though this would require duplication
  of the attributes (likely faster for items with lots of attributes, but slower
  for those with few; ideally need a way to duplicate the LISTSXP but not the
  contents - I guess wouldn't be too hard to implement).

  Alternate: use hash tables, though likely not worth it unless more than 25
  attributes, which should be rare
  */
  /*
  Here we need to `rev` so that our double loop works; if we don't rev and
  `target` has no attributes then we wouldn't check anything.  Since we know
  in that case we would fail at the first `current` attribute, maybe we should
  simplify...
  */
  SEXP prim_attr_el, sec_attr_el;
  size_t sec_attr_counted = 0, sec_attr_count = 0, prim_attr_count = 0;

  for(
    prim_attr_el = prim_attr; prim_attr_el != R_NilValue;
    prim_attr_el = CDR(prim_attr_el)
  ) {
    SEXP prim_tag = TAG(prim_attr_el);
    const char * tx = CHAR(PRINTNAME(prim_tag));
    prim_attr_count++;
    SEXP sec_attr_el_tmp = R_NilValue;

    for(
      sec_attr_el = sec_attr; sec_attr_el != R_NilValue;
      sec_attr_el = CDR(sec_attr_el)
    ) {
      if(!sec_attr_counted) sec_attr_count++;
      if(prim_tag == TAG(sec_attr_el)) {
        sec_attr_el_tmp = sec_attr_el;
        // Don't need to do full loop since we did it once already
        if(sec_attr_counted) break;
    } }
    sec_attr_counted = 1;
    sec_attr_el = sec_attr_el_tmp;

    if(prim_attr_el == R_NilValue) { // NULL attrs shouldn't be possible
      // nocov start
      error(
        "Internal Error: attribute %s is NULL for `%s`", tx,
        rev ? "current" : "target"
      );
      // nocov end
    }

    // Undo reverse now that we've gone through double loop

    SEXP tar_attr_el, tar_attr_el_val, cur_attr_el, cur_attr_el_val, tar_tag;
    if(rev) {
      tar_attr_el = sec_attr_el;
      cur_attr_el = prim_attr_el;
      tar_tag = TAG(sec_attr_el);
    } else {
      tar_attr_el = prim_attr_el;
      cur_attr_el = sec_attr_el;
      tar_tag = prim_tag;
    }
    // No match only matters if target has attrs and is not `srcref`, or in
    // strict mode

    int is_srcref =
      tar_tag != R_NilValue && !strcmp(CHAR(PRINTNAME(tar_tag)), "srcref");
    if(
      (
        (tar_attr_el == R_NilValue && set.attr_mode == 2) ||
        (tar_attr_el == R_NilValue && !is_srcref)
      ) && errs[7].success
    ) {
      errs[7].success = 0;
      errs[7].message = PROTECT(
        ALIKEC_res_msg_def(
          CSR_smprintf4(
            ALIKEC_MAX_CHAR, "%shave",
            (cur_attr_el == R_NilValue ? "" : "not "), "", "", ""
          ),
          CSR_smprintf4(ALIKEC_MAX_CHAR, "attribute \"%s\"", tx, "", "", ""),
          "", ""
      ) );
      ps++;
    } else if (is_srcref && !set.attr_mode) {
      // Don't check srcref if in default attribute mode
      continue;
    }
    cur_attr_el_val = cur_attr_el != R_NilValue ? CAR(cur_attr_el) : R_NilValue;
    tar_attr_el_val = tar_attr_el != R_NilValue ? CAR(tar_attr_el) : R_NilValue;

    // = Baseline Check ========================================================

    if(set.attr_mode && cur_attr_el_val != R_NilValue && errs[6].success) {
      res_sub = ALIKEC_compare_attributes_internal_simple(
        tar_attr_el_val, cur_attr_el_val, prim_tag, set
      );
      PROTECT(res_sub.message); ps++;
      errs[6] = res_sub;

    // = Custom Checks =========================================================

    /* see alike documentation for explanations of how the special
    attributes in this section are compared */

    } else {
      // - Class ---------------------------------------------------------------

      /* Class errors always trump all others so no need to calculate further;
      for every other error we have to keep going in case we eventually find a
      class error*/

      if(tar_tag == R_ClassSymbol) {
        SEXP cur_attr_el_val_tmp =
          PROTECT(ALIKEC_class(rev ? target : current, cur_attr_el_val));
        SEXP tar_attr_el_val_tmp =
          PROTECT(ALIKEC_class(!rev ? target : current, tar_attr_el_val));
        struct ALIKEC_res_sub class_comp = ALIKEC_compare_class(
          tar_attr_el_val_tmp, cur_attr_el_val_tmp, set
        );
        PROTECT(class_comp.message);
        ps += 3;
        is_df = class_comp.df;
        errs[0] = class_comp;
        if(!class_comp.success) break;

      // - Names ---------------------------------------------------------------

      } else if (tar_tag == R_NamesSymbol || tar_tag == R_RowNamesSymbol) {
        struct ALIKEC_res_sub name_comp =
          ALIKEC_compare_special_char_attrs_internal(
            tar_attr_el_val, cur_attr_el_val, set, 0
          );
        PROTECT(name_comp.message); ps++;
        if(!name_comp.success) {
          int is_names = tar_tag == R_NamesSymbol;
          int err_ind = is_names ? 3 : 4;
          errs[err_ind] = name_comp;

          // wrap original wrap in names/rownames

          SEXP wrap_orig = VECTOR_ELT(errs[err_ind].message, 1);
          SEXP call = PROTECT(lang2(tar_tag, R_NilValue));
          ALIKEC_wrap_around(wrap_orig, call); // modifies wrap_orig
          UNPROTECT(1);
        }
        continue;
      // - Dims ----------------------------------------------------------------

      } else if (tar_tag == R_DimSymbol && set.attr_mode == 0) {
        int err_ind = 2;

        struct ALIKEC_res_sub dim_comp = ALIKEC_compare_dims(
          tar_attr_el_val, cur_attr_el_val, target, current, set
        );
        PROTECT(dim_comp.message); ps++;

        // implicit class error upgrades to major error

        if(dim_comp.lvl) err_ind = 0;
        errs[err_ind] = dim_comp;

      // - dimnames ------------------------------------------------------------

      } else if (tar_tag == R_DimNamesSymbol) {
        struct ALIKEC_res_sub dimname_comp = ALIKEC_compare_dimnames(
          tar_attr_el_val, cur_attr_el_val, set
        );
        PROTECT(dimname_comp.message); ps++;
        errs[5] = dimname_comp;

      // - levels --------------------------------------------------------------

      } else if (tar_tag == R_LevelsSymbol) {
        struct ALIKEC_res_sub levels_comp = ALIKEC_compare_levels(
          tar_attr_el_val, cur_attr_el_val, set
        );
        PROTECT(levels_comp.message); ps++;
        errs[6] = levels_comp;

      // - tsp -----------------------------------------------------------------

      } else if (tar_tag == R_TspSymbol) {

        struct ALIKEC_res_sub ts_comp = ALIKEC_compare_ts(
          tar_attr_el_val, cur_attr_el_val, set
        );
        PROTECT(ts_comp.message); ps++;
        errs[1] = ts_comp;

      // - normal attrs --------------------------------------------------------

      } else {
        struct ALIKEC_res_sub attr_comp =
          ALIKEC_compare_attributes_internal_simple(
            tar_attr_el_val, cur_attr_el_val, prim_tag, set
          );
        PROTECT(attr_comp.message); ps++;
        errs[6] = attr_comp;
      }
  } }
  // If in strict mode, must have the same number of attributes

  if(set.attr_mode == 2 && prim_attr_count != sec_attr_count) {
    errs[7].success = 0;
    errs[7].message = PROTECT(
      ALIKEC_res_msg_def(
        "have",
        CSR_smprintf4(
          ALIKEC_MAX_CHAR, "%s attribute%s",
          CSR_len_as_chr(prim_attr_count),
          prim_attr_count != 1 ? "s" : "", "", ""
        ),
        "has",
        CSR_smprintf4(
          ALIKEC_MAX_CHAR, "%s", CSR_len_as_chr(sec_attr_count), "", "", ""
        )
    ) );
    ps++;
  }
  // Now determine which error to throw, if any

  if(!set.in_attr) {
    // nocov start
    error(
      "Internal Error: attribute depth counter corrupted; contact maintainer"
    );
    // nocov end
  }
  set.in_attr--;

  int i;
  for(i = 0; i < 8; i++) {
    if(!errs[i].success && (!rev || (rev && set.attr_mode == 2))) {
      res_attr = errs[i];
      res_attr.lvl = i;
      break;
  } }
  res_attr.df = is_df;
  UNPROTECT(ps);
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

  struct ALIKEC_settings set = ALIKEC_set_def("");
  set.attr_mode = asInteger(attr_mode);

  struct ALIKEC_res_sub comp_res =
    ALIKEC_compare_attributes_internal(target, current, set);
  return ALIKEC_res_sub_as_sxp(comp_res);
}
