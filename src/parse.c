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
Name replacement, substitutes `.` for argname and multi dots for one dot fewer.  This is specifically for the `.`.  The standard symbol substitution happens via `VALC_symb_sub`.  Note that we skip `VALC_symb_sub` for `.` when it wasn't escaped by this.
*/
SEXP VALC_name_sub(SEXP symb, SEXP arg_name) {
  if(TYPEOF(symb) != SYMSXP){
    return(symb);
  }
  // this comes out as const, but we use it as non-const, could this cause
  // problems?
  const char * symb_char = CHAR(PRINTNAME(symb));

  int i = 0, non_dot = 0;

  while(symb_char[i]) {
    if(symb_char[i] != '.') {
      non_dot = 1;
      break;
    }
    i++;
    if(i > 15000) {
      // nocov start
      error(
        "Internal Error: %s%s",
        "unexpectedly large symbol name (>15000, shouldn't happen); ",
        "contact maintainer."
      );
      // nocov end
    }
  }
  if(!non_dot && i > 0) {  // Name is only dots, and at least one
    if (i == 1) {  // one dot and an arg
      return(arg_name);
    } else if (i == 2) { // Most common multi dot scenario, escaped dot
      return(VALC_SYM_one_dot);
    } else {
      // Need to remove one dot

      size_t name_len;
      name_len = strlen(symb_char);
      char * symb_char_cpy;
      // Could allocate one less than this
      symb_char_cpy = R_alloc(name_len, sizeof(char));
      strcpy(symb_char_cpy, symb_char);               // copy to make non const
      symb_char_cpy[i - 1] = '\0';                    // shorten by one
      return(install(symb_char_cpy));
  } }
  return(symb);
}
/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */
/*
Unit testing interface
*/
SEXP VALC_name_sub_ext(SEXP symb, SEXP arg_name) {
  return(VALC_name_sub(symb, arg_name));
}

/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */
/*
  Don't need paren calls since the parsing already accounted for them

  If it encounters a call to `.(` removes that, but notes we're in mode 1 via
  the second value in the vector.
*/
SEXP VALC_remove_parens(SEXP lang) {
  SEXP mode,
       mode_0 = PROTECT(ScalarInteger(0)),
       mode_1 = PROTECT(ScalarInteger(1));
  mode = mode_0;

  while(TYPEOF(lang) == LANGSXP) {
    if(!strcmp(CHAR(PRINTNAME(CAR(lang))), "(")) {
      if(length(lang) != 2) {
        // nocov start
        error(
          "Internal Error: %s",
          "`(` call with more than one argument; contact maintainer."
        );
        // nocov end
      }
    } else if(!strcmp(CHAR(PRINTNAME(CAR(lang))), ".")) {
      if(length(lang) != 2)
        error("`.(` must be used with only one argument.");
      mode = mode_1;
    } else {
      break;
    }
    lang = CADR(lang);
  }
  SEXP res = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(res, 0, lang);
  SET_VECTOR_ELT(res, 1, mode);
  UNPROTECT(3);
  return(res);
}
/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */
/*
If a variable expands to language, sub it in and keep parsing unless the sub itself is to symbol then keep subbing until it doesn't

See VALC_name_sub too.  We substitute `.` here as well, but logic before this function is called should ensure that we only call it on `.` when it was previously `..`.

Really seems like these two functions should be merged into one so that we don't get out of sync in how we use them.
*/
SEXP VALC_sub_symbol(
  SEXP lang, struct VALC_settings set, struct track_hash * track_hash
) {
  size_t protect_i = 0;
  SEXP rho = set.env;
  while(TYPEOF(lang) == SYMSXP && lang != R_MissingArg) {
    const char * symb_chr = CHAR(PRINTNAME(lang));
    int symb_stored = VALC_add_to_track_hash(
      track_hash, symb_chr, "42", set.nchar_max
    );

    if(!symb_stored) {
      error(
        "%s%s%s%s%s",
        "Possible infinite recursion encountered when substituting symbol `",
        symb_chr,
        "`. `vetr` recursively substitutes the vetting expressions. ",
        "See `vignette('vetr', package='vetr')`, \"Non Standard Evaluation\" ",
        "section."
      );
    }
    int var_found_resolves_symbol = 0;
    if(findVar(lang, rho) != R_UnboundValue) {
      SEXP found_val = eval(lang, rho);
      SEXPTYPE found_val_type = TYPEOF(found_val);
      if(found_val_type == LANGSXP || found_val_type == SYMSXP) {
        lang = PROTECT(duplicate(found_val));
      } else PROTECT(R_NilValue);  // Balance
      var_found_resolves_symbol = found_val_type == SYMSXP;
    } else PROTECT(R_NilValue);  // Balance
    protect_i = CSR_add_szt(protect_i, 1);
    if(!var_found_resolves_symbol) break;
  }
  UNPROTECT(protect_i);
  return(lang);
}
SEXP VALC_sub_symbol_ext(SEXP lang, SEXP rho) {
  struct track_hash * track_hash = VALC_create_track_hash(64);
  struct VALC_settings set = VALC_settings_vet(R_NilValue, rho);
  return VALC_sub_symbol(lang, set, track_hash);
}
/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */

SEXP VALC_parse(SEXP lang, SEXP var_name, struct VALC_settings set) {
  SEXP lang_cpy, res, res_vec, rem_res;
  int mode;

  // Must copy since we're going to modify this
  lang_cpy = PROTECT(duplicate(lang));

  rem_res = PROTECT(VALC_remove_parens(lang_cpy));
  lang_cpy = VECTOR_ELT(rem_res, 0);
  mode = asInteger(VECTOR_ELT(rem_res, 1));

  // Hash table to track symbols to make sure  we don't end up in an infinite
  // recursion substituting symbols

  struct track_hash * track_hash =
    VALC_create_track_hash(set.track_hash_content_size);

  // Replace any variables to language objects with language, though first check
  // that we don't already have the `.`, although that would be odd even before
  // we start recursion (really this should probably be handled exclusively in
  // parse_recurse...
  //
  // mode == 1 or mode == 2 means we're in "dot" mode, just that with "2" it's
  // an actualy dot that we shouldn't substitute recursively, instead it should
  // be substituted with `name_sub`.

  if(lang_cpy == VALC_SYM_one_dot) mode = 2;
  lang_cpy = VALC_name_sub(lang_cpy, var_name);
  if(mode != 2) lang_cpy = VALC_sub_symbol(lang_cpy, set, track_hash);

  if(TYPEOF(lang_cpy) != LANGSXP) {
    res = PROTECT(ScalarInteger(mode ? 10 : 999));
  } else {
    res = PROTECT(allocList(length(lang_cpy)));
    // lang_cpy, res, are modified internally
    VALC_parse_recurse(
      lang_cpy, res, var_name, mode, R_NilValue, set, track_hash
    );
  }
  res_vec = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(res_vec, 0, lang_cpy);
  SET_VECTOR_ELT(res_vec, 1, res);
  UNPROTECT(4);
  return(res_vec);
}
SEXP VALC_parse_ext(SEXP lang, SEXP var_name, SEXP rho) {
  struct VALC_settings set = VALC_settings_vet(R_NilValue, rho);
  return VALC_parse(lang, var_name, set);
}
/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */

void VALC_parse_recurse(
  SEXP lang, SEXP lang_track, SEXP var_name, int eval_as_is,
  SEXP first_fun, struct VALC_settings set, struct track_hash * track_hash
) {
  /*
  If the object is not a language list, then return it, as part of an R vector
  list.  Otherwise, in a loop, recurse with this function on each element of the
  list, placing each an R vector list that combines this element and an
  auxillary value describing the element into a pair list that replicates in
  structure the original language list.

  Note we're purposefully modifying calls by reference so that the top level
  calls reflect the full substitution of the parse process.

  We want to modify the copy of the original call as well, need to dig into list
  with call to do so; maybe should use parallel trees, one with the expressions,
  one with the status, and the recursion is done by passing pointers to the
  current position in each tree.  Would be simpler.  Here instead we try to cram
  both trees into one by using lists and we get this mess below
  */
  static int counter = -1;
  int call_type = 999;
  counter++;  // Tracks recursion level, used for debugging


  if(TYPEOF(lang) != LANGSXP) {  // Not a language expression
    // nocov start
    error("Internal Error: unexpectedly encountered a non-language object");
    // nocov end
  }
  const char * call_symb;

  // Determine if we're dealing with a special code, and if so determine what
  // type and record

  call_symb = CHAR(PRINTNAME(CAR(lang)));
  if(eval_as_is) {
    call_type = 10;
  } else if(!strcmp(call_symb, "&&")) {
    call_type = 1;
  } else if(!strcmp(call_symb, "||")) {
    call_type = 2;
  }
  SETCAR(lang_track, ScalarInteger(call_type));           // Track type of call

  if(first_fun == R_NilValue && call_type >= 10) {
    // First time we're no longer parsing && / ||, record so that we can then
    // modify call_type if we run into a `.` while parsing expression

    first_fun = lang_track;
  }
  lang = CDR(lang);
  lang_track = CDR(lang_track);
  call_type = 999; // Reset for sub-elements

  // Loop through remaining elements of call; if any are calls, recurse,
  // otherwise sub for dots and record as templates (999).  Stuff here shouldn't
  // need to be PROTECTed since it is pointed at but PROTECTED stuff.

  while(lang != R_NilValue) {
    int eval_as_is_internal = eval_as_is;
    // Remove parens removes parens and `.` calls, and indicates whether a `.(`
    // call was encountered through the second value in the return list.  This
    // means that all elements of this language object henceforth should be
    // evaled as is.  This is distinct to encountering a `.` which would only
    // affect that element.

    SEXP rem_parens = PROTECT(VALC_remove_parens(CAR(lang)));
    if(asInteger(VECTOR_ELT(rem_parens, 1)) || eval_as_is_internal) {
      eval_as_is_internal = 1;
    } else {
      eval_as_is_internal = 0;
    }
    SEXP lang_car = VECTOR_ELT(rem_parens, 0);

    // Replace any variables to language objects with language

    int is_one_dot = (lang_car == VALC_SYM_one_dot);
    lang_car = PROTECT(VALC_name_sub(lang_car, var_name));
    if(!is_one_dot) lang_car = VALC_sub_symbol(lang_car, set, track_hash);
    UNPROTECT(1);
    SETCAR(lang, lang_car);
    UNPROTECT(1);

    if(TYPEOF(lang_car) == LANGSXP && !is_one_dot) {
      SEXP track_car = allocList(length(lang_car));
      SETCAR(lang_track, track_car);

      // each time we exit from a recursion, we should reset the hash so that we
      // don't mistakenly tag symbol collisions that occur on different branches
      // of the parse tree

      size_t substitute_level = track_hash->idx;
      VALC_parse_recurse(
        lang_car, CAR(lang_track), var_name, eval_as_is_internal,
        first_fun, set, track_hash
      );
      VALC_reset_track_hash(track_hash, substitute_level);
    } else {
      int new_call_type = call_type;
      if(is_one_dot || eval_as_is_internal) {
        if(first_fun != R_NilValue)
          SETCAR(first_fun, ScalarInteger(10));
        new_call_type = 10;
      }
      SETCAR(lang_track, ScalarInteger(new_call_type));
    }
    lang = CDR(lang);
    lang_track = CDR(lang_track);
  }
  if(lang == R_NilValue && lang_track != R_NilValue) {
    // nocov start
    error(
      "Internal Error: %s",
      "unsychronized call tree and call tracking tree; contact maintainer."
    );
    // nocov end
  }
  counter--;

  // Don't return anything as all is done by modifying `lang` and `lang_track`
}
