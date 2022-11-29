/*
Copyright (C) 2022 Brodie Gaslam

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
Initialize return object
*/
/*
Moves pointer on language object to skip any `(` calls since those are
already accounted for in parsing and as such don't add anything.

Returns a list/vector with a pointer to the updated lanaguage object position
and an integer indicating how many parentheses were skipped
*/

SEXP ALIKEC_skip_paren(SEXP lang) {
  int i = 0;
  SEXP res = PROTECT(allocVector(VECSXP, 2));
  if(TYPEOF(lang) == LANGSXP) {
    while(
      CAR(lang) == ALIKEC_SYM_paren_open && CDR(CDR(lang)) == R_NilValue
    ) {
      lang = CADR(lang);
      i++;
      if(i < 0) {
        // nocov start
        error(
          "Internal Error: %s; contact maintainer.",
          "exceeded language recursion depth when skipping parens"
        );
        // nocov end
      }
  } }
  SET_VECTOR_ELT(res, 0, lang);
  SET_VECTOR_ELT(res, 1, ScalarInteger(i));
  UNPROTECT(1);
  return(res);
}

// - Anonymize Formula ---------------------------------------------------------

/* Look up symbol in hash table, if already present, return the anonymized
version of the symbol.  If not, add to the hash table.

symb the symbol to lookup
hash the hash table
varnum used to generate the anonymized variable name
*/

const char * ALIKEC_symb_abstract(
  SEXP symb, pfHashTable * hash, size_t * varnum, struct VALC_settings set
) {
  const char * symb_chr = CHAR(PRINTNAME(symb));
  // really shouldn't have to do this, but can't be bothered re-defining the
  // hash library
  const char * symb_abs = pfHashFind(hash, (char *) symb_chr);
  if(symb_abs == NULL) {
    symb_abs = CSR_smprintf4(
      set.nchar_max, "a%s", CSR_len_as_chr(*varnum), "", "", ""
    );
    pfHashSet(hash, (char *) symb_chr, symb_abs);
    (*varnum)++;
  }
  return symb_abs;
}
/*
Try to find function in env and return function if it exists, R_NilValue
otherwise

@param call the function we will ultimately match
@param env an environment to start the lookup
*/
SEXP ALIKEC_get_fun(SEXP call, SEXP env) {
  SEXP fun = CAR(call);
  switch(TYPEOF(fun)) {
    case CLOSXP:
      return fun;
      break;
    case SYMSXP:
      {
        // Assuming no GC happens in next couple of steps
        SEXP fun_def = ALIKEC_findFun(fun, env);
        if(TYPEOF(fun_def) == CLOSXP) return fun_def;
      }
      break;
  }
  return R_NilValue;
}
/*
@param match_call a preconstructed call to retrieve the function; needed because
  can't figure out a way to create preconstructed call in init without
  sub-components getting GCed
*/
SEXP ALIKEC_match_call(
  SEXP call, SEXP match_call, SEXP env
) {
  SEXP fun = PROTECT(ALIKEC_get_fun(call, env));
  if(fun == R_NilValue) {
    UNPROTECT(1);
    return call;
  }
  // remember, match_call is pre-defined as: match.call(def, quote(call)), also,
  // in theory should never be SHARED since it is a SEXP created in C code only
  // for internal use.

  if (MAYBE_SHARED(match_call)) PROTECT(match_call = duplicate(match_call));
  else PROTECT(R_NilValue);

  SETCADR(match_call, fun);
  SETCADR(CADDR(match_call), call);
  int tmp = 0;
  int * err =& tmp;
  SEXP res = PROTECT(R_tryEvalSilent(match_call, env, err));
  UNPROTECT(3);
  if(* err) return call; else return res;
}
/*
Handle language object comparison

Note that we always pass cur_par instead of current so that we can modify the
original call (mostly by using `match.call` on it)
*/
struct ALIKEC_res ALIKEC_lang_obj_compare(
  SEXP target, SEXP cur_par, pfHashTable * tar_hash,
  pfHashTable * cur_hash, pfHashTable * rev_hash, size_t * tar_varnum,
  size_t * cur_varnum, int formula, SEXP match_call, SEXP match_env,
  struct VALC_settings set, struct ALIKEC_rec_track rec
) {
  SEXP current = CAR(cur_par);
  struct ALIKEC_res res = ALIKEC_res_init();
  res.dat.rec = rec;

  // Skip parens and increment recursion; not we don't track recursion level
  // for target

  SEXP cur_skip_paren = PROTECT(ALIKEC_skip_paren(current));
  SEXP tar_skip_paren = PROTECT(ALIKEC_skip_paren(target));

  // Need to reset all variables so we work on the paren less version

  current = VECTOR_ELT(cur_skip_paren, 0);
  SEXP cur_par_dup = PROTECT(duplicate(cur_par));
  SETCAR(cur_par_dup, current);

  int i, i_max = asInteger(VECTOR_ELT(cur_skip_paren, 1));

  PROTECT(res.wrap);   // Dummy PROTECT

  for(i = 0; i < i_max; i++) {
    res.dat.rec = ALIKEC_rec_inc(res.dat.rec);
  }
  target = VECTOR_ELT(tar_skip_paren, 0);

  SEXPTYPE tsc_type = TYPEOF(target), csc_type = TYPEOF(current);
  res.success = 0;  // assume fail until shown otherwise

  if(target == R_NilValue) {// NULL matches anything
    res.success = 1;
  } else if(tsc_type == SYMSXP && csc_type == SYMSXP) {
    const char * tar_abs = ALIKEC_symb_abstract(
      target, tar_hash, tar_varnum, set
    );
    const char * cur_abs = ALIKEC_symb_abstract(
      current, cur_hash, cur_varnum, set
    );
    // reverse hash to get what symbol should be in case of error
    const char * rev_symb = pfHashFind(rev_hash, tar_abs);
    const char * csc_text = CHAR(PRINTNAME(current));
    if(rev_symb == NULL) {
      rev_symb = (char *) csc_text;
      pfHashSet(rev_hash, cur_abs, rev_symb);
    }
    if(strcmp(tar_abs, cur_abs)) {
      res.success = 0;
      if(*tar_varnum > *cur_varnum) {
        res.dat.strings.tar_pre = "not be";
        res.dat.strings.target[0] = "`%s`";
        res.dat.strings.target[1] = csc_text;
        res.dat.strings.current[1] = ""; // gcc-10
      } else {
        res.dat.strings.target[0] = "`%s`";
        res.dat.strings.target[1] = rev_symb;
        res.dat.strings.current[0] = "`%s`";
        res.dat.strings.current[1] = csc_text;
      }
    } else res.success = 1;
  } else if (tsc_type == LANGSXP && csc_type != LANGSXP) {
    res.success = 0;
    res.dat.strings.target[0] = "a call to `%s`";
    res.dat.strings.target[1] = ALIKEC_deparse_chr(CAR(target), -1, set);
    res.dat.strings.current[0] =  "\"%s\"";
    res.dat.strings.current[1] = type2char(csc_type);
  } else if (tsc_type != LANGSXP && csc_type == LANGSXP) {
    res.success = 0;
    res.dat.strings.target[0] =  "\"%s\"";
    res.dat.strings.target[1] = type2char(tsc_type);
    res.dat.strings.current[0] =  "\"%s\"";
    res.dat.strings.current[1] = type2char(csc_type);
  } else if (tsc_type == LANGSXP) {
    // Note how we pass cur_par and not current so we can modify cur_par
    // this should be changed since we don't use that feature any more
    UNPROTECT(1);
    res = ALIKEC_lang_alike_rec(
      target, cur_par_dup, tar_hash, cur_hash, rev_hash, tar_varnum,
      cur_varnum, formula, match_call, match_env, set, res.dat.rec
    );
    PROTECT(res.wrap);
  } else if(tsc_type == SYMSXP || csc_type == SYMSXP) {
    res.success = 0;
    res.dat.strings.target[0] =  "\"%s\"";
    res.dat.strings.target[1] = type2char(tsc_type);
    res.dat.strings.current[0] =  "\"%s\"";
    res.dat.strings.current[1] = type2char(csc_type);
  } else if (formula && !R_compute_identical(target, current, 16)) {
    // Maybe this shouldn't be "identical", but too much of a pain in the butt
    // to do an all.equals type comparison

    // could have constant vs. language here, right?

    res.success = 0;
    res.dat.strings.tar_pre = "have";
    res.dat.strings.target[1] =  "identical constant values";
    res.dat.strings.current[1] = ""; // gcc-10
  } else res.success = 1;

  // Deal with index implications of skiping parens, note + 2 because we need
  // +1 for zero index, and then another +1 to reference contents of parens

  if(!res.success) {
    for(i = 0; i < i_max; i++) {
      res.dat.rec = ALIKEC_rec_ind_num(res.dat.rec, i + 2);
      res.dat.rec = ALIKEC_rec_dec(res.dat.rec);
    }
    if(res.wrap == R_NilValue) res.wrap = allocVector(VECSXP, 2);
  }
  UNPROTECT(4);
  return res;
}

/*
Creates a copy of the call mapping objects to a deterministic set of names
based on the order in which they appear in the call

Here we use a hash table to identify whether a symbol already showed up or not.
This is probably faster if langauge object has 25 or more elements, so may
eventually want to add logic that choses path based on how many elements.

If return value is zero length string then comparison succeeded, otherwise
return value is error message.  Note that we also return information by modifying
the `cur_par` argument by reference.  We either mark the token the error message
refers to by wrapping it in ``{}``, or blow it away to indicate we don't want
the final error message to try to point out where the error occurred (this is
typically the case when the error is not specific to a particular part of the
call).
*/

struct ALIKEC_res ALIKEC_lang_alike_rec(
  SEXP target, SEXP cur_par, pfHashTable * tar_hash, pfHashTable * cur_hash,
  pfHashTable * rev_hash, size_t * tar_varnum, size_t * cur_varnum, int formula,
  SEXP match_call, SEXP match_env, struct VALC_settings set,
  struct ALIKEC_rec_track rec
) {
  SEXP current = CAR(cur_par);

  // If not language object, run comparison

  struct ALIKEC_res res = ALIKEC_res_init();
  res.dat.rec = rec;

  if(TYPEOF(target) != LANGSXP || TYPEOF(current) != LANGSXP) {
    res =  ALIKEC_lang_obj_compare(
      target, cur_par, tar_hash, cur_hash, rev_hash, tar_varnum,
      cur_varnum, formula, match_call, match_env, set, res.dat.rec
    );
  } else {
    // If language object, then recurse

    res.dat.rec = ALIKEC_rec_inc(res.dat.rec);

    SEXP tar_fun = CAR(target), cur_fun = CAR(current);

    // Actual fun call must match exactly, unless NULL

    if(tar_fun != R_NilValue && !R_compute_identical(tar_fun, cur_fun, 16)) {
      res.success = 0;
      res.dat.rec = ALIKEC_rec_ind_num(res.dat.rec, 1);

      res.dat.strings.target[0] = "a call to `%s`";
      res.dat.strings.target[1] = ALIKEC_deparse_chr(CAR(target), -1, set);

      res.dat.strings.current[0] = "a call to `%s`";
      res.dat.strings.current[1] = ALIKEC_deparse_chr(CAR(current), -1, set);

    } else if (CDR(target) != R_NilValue) {
      // Zero length calls match anything, so only come here if target is not
      // Nil

      // Match the calls before comparison; small inefficiency below since we
      // know that target and current must be the same fun; we shouldn't need
      // to retrieve it twice as we do now

      int use_names = 1;
      if(match_env != R_NilValue && set.lang_mode != 1) {
        target = PROTECT(ALIKEC_match_call(target, match_call, match_env));
        current = PROTECT(ALIKEC_match_call(current, match_call, match_env));
        SETCAR(cur_par, current);  // ensures original call is matched
        // Can't be sure that names will match up with call as originally
        // submitted
        use_names = 0;
      } else {
        PROTECT(PROTECT(R_NilValue)); // stack balance
      }
      SEXP tar_sub, cur_sub, cur_sub_tag, tar_sub_tag,
        prev_tag = R_UnboundValue;
      R_xlen_t arg_num;

      for(
        tar_sub = CDR(target), cur_sub = CDR(current), arg_num = 0;
        tar_sub != R_NilValue && cur_sub != R_NilValue;
        tar_sub = CDR(tar_sub), cur_sub = CDR(cur_sub), prev_tag = cur_sub_tag,
        arg_num++
      ) {
        if(arg_num > R_XLEN_T_MAX - 1) {
          // nocov start
          error(
            "Internal Error: %s; contact maintainer.",
            "exceeded allowable call length"
          );
          // nocov end
        }
        // Check tags are compatible; NULL tag in target allows any tag in
        // current

        cur_sub_tag = TAG(cur_sub);
        tar_sub_tag = TAG(tar_sub);

        int update_rec_ind = 0;

        if(tar_sub_tag != R_NilValue && tar_sub_tag != cur_sub_tag) {
          char * prev_tag_msg = "as first argument";
          if(prev_tag != R_UnboundValue) {
            if(prev_tag == R_NilValue) {
              prev_tag_msg = CSR_smprintf4(
                set.nchar_max, "after argument %s", CSR_len_as_chr(arg_num),
                "", "", ""
              );
            } else {
              prev_tag_msg = CSR_smprintf4(
                set.nchar_max, "after argument `%s`", CHAR(PRINTNAME(prev_tag)),
                "", "", ""
          );} }
          res.success = 0;

          res.dat.strings.tar_pre = "have";
          res.dat.strings.target[0] =  "argument `%s` %s";
          res.dat.strings.target[1] = CHAR(PRINTNAME(TAG(tar_sub)));
          res.dat.strings.target[2] = prev_tag_msg;
          res.dat.strings.cur_pre = "has";

          if(TAG(cur_sub) == R_NilValue) {
            res.dat.strings.current[1] = "unnamed argument";
          } else {
            res.dat.strings.current[0] =  "`%s`";
            res.dat.strings.current[1] =  CHAR(PRINTNAME(TAG(cur_sub)));
          }
        } else {
          // Note that `lang_obj_compare` kicks off recursion as well, and
          // skips parens

          SEXP tar_sub_car = CAR(tar_sub);
          res = ALIKEC_lang_obj_compare(
            tar_sub_car, cur_sub, tar_hash, cur_hash, rev_hash,
            tar_varnum, cur_varnum, formula, match_call, match_env, set,
            res.dat.rec
          );
          update_rec_ind = 1;
        }
        // Update recursion indices and exit loop; keep in mind that this is a
        // call so first element is fun, hence `arg_num + 2`

        if(!res.success) {
          if(update_rec_ind) {
            if(cur_sub_tag != R_NilValue && use_names)
              res.dat.rec =
                ALIKEC_rec_ind_chr(res.dat.rec, CHAR(PRINTNAME(cur_sub_tag)));
            else
              res.dat.rec =
                ALIKEC_rec_ind_num(res.dat.rec, arg_num + 2);
          }
          break;
        }
      }
      if(res.success) {
        // Make sure that we compared all items; missing R_NilValue here means
        // one of the calls has more items

        R_xlen_t tar_len, cur_len;
        tar_len = cur_len = arg_num;
        if(tar_sub != R_NilValue || cur_sub != R_NilValue) {
          while(tar_sub != R_NilValue) {
            tar_len++;
            tar_sub = CDR(tar_sub);
          }
          while(cur_sub != R_NilValue) {
            cur_len++;
            cur_sub = CDR(cur_sub);
          }
          res.success = 0;
          res.dat.strings.tar_pre = "have";
          res.dat.strings.target[0] = "%s arguments";
          res.dat.strings.target[1] = CSR_len_as_chr(tar_len);
          res.dat.strings.cur_pre = "has";
          res.dat.strings.current[1] = CSR_len_as_chr(cur_len);
        }
      }
      target = current = R_NilValue;

      UNPROTECT(2);
    }
    res.dat.rec = ALIKEC_rec_dec(res.dat.rec);
  }
  return res;
}
/*
Compare language objects.

This is a semi internal function used by the internal language comparison
mechanism as well as the external testing functions.

Determine whether objects should be compared as calls or as formulas; the main
difference in treatment is that calls are match-called if possible, and also
that for calls constants need not be the same

Return a list (vector) with the status, error message, the matched language
object, the original language object, and index within the langauge object of
the problem if there is one (relative to the matched object)
*/

SEXP ALIKEC_lang_alike_core(
  SEXP target, SEXP current, struct VALC_settings set
) {
  SEXP match_env = set.env;
  SEXPTYPE tar_type = TYPEOF(target), cur_type = TYPEOF(current);
  int tar_is_lang =
    tar_type == LANGSXP || tar_type == SYMSXP || tar_type == NILSXP;
  int cur_is_lang =
    cur_type == LANGSXP || cur_type == SYMSXP || cur_type == NILSXP;
  if(!(tar_is_lang && cur_is_lang))
    error("Arguments must be LANGSXP, SYMSXP, or R_NilValue");

  if(TYPEOF(match_env) != ENVSXP && match_env != R_NilValue)
    error("Argument `match.call.env` must be an environment or NULL");

  /*
  Create persistent objects for use throught recursion; these are the hash
  tables that are used to keep track of names as they show up as we recurse
  through the language objects
  */

  pfHashTable * tar_hash = pfHashCreate(NULL);
  pfHashTable * cur_hash = pfHashCreate(NULL);
  pfHashTable * rev_hash = pfHashCreate(NULL);
  size_t tartmp = 0, curtmp=0;
  size_t * tar_varnum = &tartmp;
  size_t * cur_varnum = &curtmp;

  // Can't figure out how to do this on init; cost ~60ns
  SEXP match_call = PROTECT(
    list3(
      ALIKEC_SYM_matchcall, R_NilValue,
      list2(R_QuoteSymbol, R_NilValue)
  ) );
  SET_TYPEOF(match_call, LANGSXP);
  SET_TYPEOF(CADDR(match_call), LANGSXP);

  int formula = 0;

  // Determine if it is a formular or not

  SEXP class = PROTECT(getAttrib(target, R_ClassSymbol));
  if(
    class != R_NilValue && TYPEOF(class) == STRSXP &&
    !strcmp("formula", CHAR(STRING_ELT(class, XLENGTH(class) - 1))) &&
    CAR(target) == ALIKEC_SYM_tilde
  ) {
    formula = 1;
  }
  UNPROTECT(1);
  // Check if alike; originally we would modify a copy of current, which is
  // why we send curr_cpy_par

  SEXP curr_cpy_par = PROTECT(list1(duplicate(current)));
  struct ALIKEC_rec_track rec = ALIKEC_rec_track_init();
  struct ALIKEC_res res = ALIKEC_lang_alike_rec(
    target, curr_cpy_par, tar_hash, cur_hash, rev_hash, tar_varnum, cur_varnum,
    formula, match_call, match_env, set, rec
  );
  // Save our results in a SEXP to simplify testing
  const char * names[6] = {
    "success", "message", "call.match", "call.ind", "call.ind.sub.par",
    "call.orig"
  };
  SEXP res_fin = PROTECT(allocVector(VECSXP, 6));
  SEXP res_names = PROTECT(allocVector(STRSXP, 6));

  for(int i = 0; i < 6; i++) SET_STRING_ELT(res_names, i, mkChar(names[i]));

  setAttrib(res_fin, R_NamesSymbol, res_names);
  SET_VECTOR_ELT(res_fin, 0, ScalarLogical(res.success));

  if(!res.success) {
    SEXP rec_ind = PROTECT(ALIKEC_rec_ind_as_lang(res.dat.rec));

    SEXP res_msg = PROTECT(allocVector(VECSXP, 2));
    SEXP res_msg_names = PROTECT(allocVector(STRSXP, 2));
    SET_VECTOR_ELT(res_msg, 0, ALIKEC_res_strings_to_SEXP(res.dat.strings));
    if(res.wrap == R_NilValue) {
      res.wrap = PROTECT(allocVector(VECSXP, 2));
    } else PROTECT(R_NilValue);
    SET_VECTOR_ELT(res_msg, 1, res.wrap);
    SET_STRING_ELT(res_msg_names, 0, mkChar("message"));
    SET_STRING_ELT(res_msg_names, 1, mkChar("wrap"));
    setAttrib(res_msg, R_NamesSymbol, res_msg_names);
    SET_VECTOR_ELT(res_fin, 1, res_msg);
    UNPROTECT(3);

    SET_VECTOR_ELT(res_fin, 2, CAR(curr_cpy_par));
    SET_VECTOR_ELT(res_fin, 3, VECTOR_ELT(rec_ind, 0));
    SET_VECTOR_ELT(res_fin, 4, VECTOR_ELT(rec_ind, 1));
    SET_VECTOR_ELT(res_fin, 5, current);
    UNPROTECT(1);
  }
  UNPROTECT(4);
  return res_fin;
}
/*
  Translate result into res_sub for use by alike

  Probalby some inefficiency in the C -> SEXP -> C translations going on; this
  is happening mostly for legacy reason so should probably clean up to stick to
  C at some point.  One of the changes (amongst others) is that we no longer
  care about recording the call / language that caused the problem since we're
  refering directly to the original object
*/
struct ALIKEC_res ALIKEC_lang_alike_internal(
  SEXP target, SEXP current, struct VALC_settings set
) {
  SEXP lang_res = PROTECT(ALIKEC_lang_alike_core(target, current, set));

  struct ALIKEC_res res = ALIKEC_res_init();
  if(asInteger(VECTOR_ELT(lang_res, 0))) {
    PROTECT(res.wrap);  // stack balance
  } else {
    res.success = 0;

    SEXP message = PROTECT(VECTOR_ELT(lang_res, 1));
    SEXP msg_txt = VECTOR_ELT(message, 0);

    res.dat.strings.tar_pre = CHAR(STRING_ELT(msg_txt, 0));
    res.dat.strings.target[1] = CHAR(STRING_ELT(msg_txt, 1));
    res.dat.strings.cur_pre = CHAR(STRING_ELT(msg_txt, 2));
    res.dat.strings.current[1] = CHAR(STRING_ELT(msg_txt, 3));

    // Deal with wrap

    SEXP lang_ind = VECTOR_ELT(lang_res, 3);
    SEXP lang_ind_sub = VECTOR_ELT(lang_res, 4);

    SEXP wrap = VECTOR_ELT(message, 1);
    SET_VECTOR_ELT(wrap, 0, lang_ind);
    SET_VECTOR_ELT(wrap, 1, lang_ind_sub);
    res.wrap = wrap;
  }
  UNPROTECT(2);
  return res;
}
/*
For testing purposes
*/
SEXP ALIKEC_lang_alike_ext(
  SEXP target, SEXP current, SEXP match_env
) {
  struct VALC_settings set = VALC_settings_init();
  set.env = match_env;
  return ALIKEC_lang_alike_core(target, current, set);
}

SEXP ALIKEC_lang_alike_chr_ext(
  SEXP target, SEXP current, SEXP match_env
) {
  struct VALC_settings set = VALC_settings_init();
  set.env = match_env;
  struct ALIKEC_res res = ALIKEC_lang_alike_internal(target, current, set);
  PROTECT(res.wrap);
  SEXP res_str;
  if(!res.success) {
    res_str = PROTECT(ALIKEC_res_strings_to_SEXP(res.dat.strings));
  } else {
    res_str = PROTECT(mkString(""));
  }
  UNPROTECT(2);
  return res_str;
}
