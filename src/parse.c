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

#include "validate.h"

/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */
/*
Un escape symbol names made up of purely dots symbols by removing one dot.
This is to implement the dot escaping syntax.
*/
static SEXP unescape_dots(const char * symb_char) {
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
      error("Internal Error: Cannot dot unescape single `.`."); // nocov
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
  return(R_NilValue);
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
    SEXP fun_symb = CAR(lang);
    if(TYPEOF(fun_symb) == SYMSXP) {
      const char * sym_name = CHAR(PRINTNAME(fun_symb));
      if(!strcmp(sym_name, "(")) {
        if(length(lang) != 2) {
          // nocov start
          error(
            "Internal Error: %s",
            "`(` call with more than one argument; contact maintainer."
          );
          // nocov end
        }
      } else if(!strcmp(sym_name, ".")) {
        if(length(lang) != 2)
          error("`.(` must be used with only one argument.");
        mode = mode_1;
      } else {
        break;
      }
      lang = CADR(lang);
    } else break;
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
If a variable expands to language, sub it in and keep parsing unless the sub
itself is to symbol then keep subbing until it doesn't.  This handles things
like INT.1.POS, etc.

See VALC_name_sub too.  We substitute `.` here as well, but logic before this
function is called should ensure that we only call it on `.` when it was
previously `..`.

Really seems like these two functions should be merged into one so that we don't
get out of sync in how we use them.

@param arg_name used primarily for `vetr`, or if in `vet` current is just a
  single variable name, allows us to verify that we're not accidentally
  referencing the variable name in a vetting token that is supposed to be a
  standard token.
*/
SEXP VALC_sub_symbol(
  SEXP lang, struct VALC_settings set, struct track_hash * track_hash,
  SEXP arg_tag
) {
  int check_arg_tag = TYPEOF(arg_tag) == SYMSXP;
  SEXP rho = set.env;

  // Each iteration may create a SEXP, but we only care about the last SEXP , so
  // we will repeatedly PROTECT the last SEXP at the location below

  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(R_NilValue, &ipx);

  while(TYPEOF(lang) == SYMSXP && lang != R_MissingArg) {
    if(check_arg_tag && lang == arg_tag) {
      error(
        "vet/vetr usage error: found symbol `%s` %s%s%s",
        CHAR(PRINTNAME(arg_tag)),
        "in vetting token for object with the same symbol.  Please use `.` ",
        "to reference the object being checked by the vetting token (e.g. ",
        "use `vet(. > 0, x)` instead of `vet(x > 0, x)`)"
      );
    }
    const char * symb_chr = CHAR(PRINTNAME(lang));

    // Escape dots: `..` becomes `.`, but at a regular variable not the vetr `.`
    SEXP symb_unesc = unescape_dots(symb_chr);
    if(symb_unesc != R_NilValue) {
      symb_chr = CHAR(PRINTNAME(symb_unesc));
      lang = symb_unesc;
    }
    // Make sure we don't get stuck in an infinite substitution loop
    int symb_stored = VALC_add_to_track_hash(
      track_hash, symb_chr, "42", set.nchar_max
    );
    if(!symb_stored) { // 0 means symbol already existed.
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
      SEXP found_val = PROTECT(eval(lang, rho));
      SEXPTYPE found_val_type = TYPEOF(found_val);
      if(found_val_type == LANGSXP || found_val_type == SYMSXP) {
        REPROTECT(lang = duplicate(found_val), ipx);
      }
      var_found_resolves_symbol = found_val_type == SYMSXP;
      UNPROTECT(1);
    }
    if(!var_found_resolves_symbol) break;
  }
  UNPROTECT(1);
  return(lang);
}
SEXP VALC_sub_symbol_ext(SEXP lang, SEXP rho) {
  struct track_hash * track_hash = VALC_create_track_hash(64);
  struct VALC_settings set = VALC_settings_vet(R_NilValue, rho);
  return VALC_sub_symbol(lang, set, track_hash, R_NilValue);
}
/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */
/*
 * Parse Validator Language
 *
 * Create a structure with the same recusive topology as the call, but for each
 * element replace with two elements, the original element, along with the
 * designation of the element (is it an `&&` (1), `||` (2), as-is (10, i.e.
 * contains dot), or alike token (999)).
 *
 * @param arg_tag the parameter name being validated, apparently `arg_lang` is
 *   the full substituted call, not just the symbol (and per #109 could even be
 *   a live object in cases of e.g. `do.call` invocation).
 */

SEXP VALC_parse(
  SEXP lang, SEXP arg_lang, struct VALC_settings set, SEXP arg_tag
) {
  SEXP lang_cpy, lang2_cpy, res_vec;

  // Must copy since we're going to modify this

  lang_cpy = PROTECT(duplicate(lang));
  lang2_cpy = PROTECT(duplicate(lang_cpy));

  // Hash table to track symbols to make sure  we don't end up in an infinite
  // recursion substituting symbols

  struct track_hash * track_hash =
    VALC_create_track_hash(set.track_hash_content_size);
  struct track_hash * track_hash2 =
    VALC_create_track_hash(set.track_hash_content_size);

  // Wrap all the objects with a handle so that we can modify them even at the
  // top-most level and retrieve the modified version.
  SEXP lang_track_handle = PROTECT(allocList(1));
  SETCAR(lang_track_handle, allocList(length(lang_cpy)));
  SEXP lang_handle = PROTECT(allocList(1));
  SETCAR(lang_handle, lang_cpy);
  SEXP lang2_handle = PROTECT(allocList(1));
  SETCAR(lang2_handle, lang2_cpy);

  // First three parameters are modified by reference
  VALC_parse_recurse(
    lang_handle, lang2_handle, lang_track_handle, arg_lang, 0, set, track_hash,
    track_hash2, arg_tag
  );

  res_vec = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(res_vec, 0, CAR(lang_handle));
  SET_VECTOR_ELT(res_vec, 1, CAR(lang_track_handle));
  SET_VECTOR_ELT(res_vec, 2, CAR(lang2_handle));
  UNPROTECT(6);
  return(res_vec);
}
SEXP VALC_parse_ext(SEXP lang, SEXP arg_lang, SEXP rho) {
  struct VALC_settings set = VALC_settings_vet(R_NilValue, rho);
  return VALC_parse(lang, arg_lang, set, R_NilValue);
}
/* -------------------------------------------------------------------------- *\
\* -------------------------------------------------------------------------- */

/*
 * Parsing does the following:
 *
 * Processes lang/lang2 and:
 *
 * * Substitutes symbols like INT.1.POS.
 * * Removes superfluous parentheses (including `.()`)
 * * Identifies which top-level expressions contain `.` (or are wrapped in
 *    `.()`).
 * * Classifies top-level expressions on whether they should be evaluated as is
 *   to check for truth, or whether they should be compared to the parameter
 *   under validation with `alike`.
 *
 * `lang`/`lang2` are modified by reference.  Top-level expression types (i.e.
 * those that aren't && or ||) are classified in `lang_track`.
 *
 * A bit wasteful that we have both `lang` and `lang2`, but we realized we need
 * them because `lang` is the langauge that gets evaluaed, and `lang2` the one
 * that we use for error reporting.  We need them to be different because they
 * need to be evaluated in different envs to make sense, and only by evaluating
 * `lang` in the function frame do we avoid a potential duplicate evaluation (if
 * we evalute `lang2` in parent.frame(2), then we will also evaluate it when we
 * force the promise).
 *
 * Even worse, turns out that we only need the `lang2` business for `vetr`,
 * `vet`/`tev` are fine with the original logic, so now we have the entire
 * duplicated version of the `lang2` logic that we throw away for `vet`/`tev`.
 *
 * @param lang the original call that where we will substitute `.` with the
 *   corresponding parameter
 * @param lang2 the original call that where we will substitute `.` with the
 *   corresponding substituted language used for the corresponding parameter;
 *   this is used to produce more descriptive errors.
 * @param token whether we part of a token, i.e. any expression that is a child
 *   of the first non-(&& / ||) call in any branch of the vetr expression.  At
 *   that point we're just substituting symbols and hunting for `.`.
 * @param lang_track a pairlist that mirrors the call structure up to the first
 *   non-(&& / ||) call, but contains in CARS the type of token each element is.
 * @return whether lang contains a `.` or not.
 */

int VALC_parse_recurse(
  SEXP lang, SEXP lang2, SEXP lang_track, SEXP arg_lang,
  int token, struct VALC_settings set,
  struct track_hash * track_hash, struct track_hash * track_hash2,
  SEXP arg_tag
) {
  // We really should use constants instead of 1, 2, 10, 999...
  int call_type = 999;   // Assume `alike` token until proven otherwise
  int has_dot = 0;
  int token0 = token;
  int prt = 0;

  // Keep track of handles so we can modify their CAR without breaking the link
  // to the pointers in the calling function.

  SEXP lang_0 = lang; lang = CAR(lang_0);
  SEXP lang2_0 = lang2; lang2 = CAR(lang2_0);
  SEXP lang_track_0 = lang_track; lang_track = CAR(lang_track_0);

  // Record symbol collision level (see end of function where we restore it)
  size_t substitute_level = track_hash->idx;
  size_t substitute_level2 = track_hash2->idx;

  // static int counter = -1;// Tracks recursion level, used for debugging
  // counter++;  Rprintf(" ^^ Rec level %d ---------------\n\n", counter);

  // Remove parens removes parens and `.(` calls, and indicates whether a `.(`
  // call was encountered through the second value in the return list.  This
  // means that all elements of this language object henceforth should be
  // evaled as is.  This is distinct to encountering a `.` which would only
  // affect that element.

  SEXP rem_parens = PROTECT(VALC_remove_parens(lang)); prt++;
  SEXP rem2_parens = PROTECT(VALC_remove_parens(lang2)); prt++;

  int is_dot_call = asInteger(VECTOR_ELT(rem_parens, 1));
  has_dot = has_dot || is_dot_call;

  lang = VECTOR_ELT(rem_parens, 0);
  lang2 = VECTOR_ELT(rem2_parens, 0);
  int is_one_dot = (lang == VALC_SYM_one_dot);
  has_dot = has_dot || is_one_dot;

  if(has_dot) call_type = 10;

  // Symbol substitutions

  if(TYPEOF(lang) == SYMSXP) {
    if(is_one_dot) {
      // A true `.`, sub with expression to validate, and no further subs.
      lang = arg_tag;   // always a symbol
      lang2 = arg_lang; // could be language, or any R object really
    } else {
      // Expand symbols bound to language into that language (e.g. INT.1, etc.).
      lang = PROTECT(VALC_sub_symbol(lang, set, track_hash, arg_tag)); prt++;
      lang2 = PROTECT(VALC_sub_symbol(lang2, set, track_hash2, arg_tag)); prt++;
    }
  }
  // Re-initialize token type tracking list now that substitution happened

  lang_track = PROTECT(allocList(length(lang))); prt++;

  // Record starting position of language as we'll move through the elements

  SEXP lang_iter = lang;
  SEXP lang2_iter = lang2;
  SEXP lang_track_iter = lang_track;

  if(!token && TYPEOF(lang) == LANGSXP) {
    // Determine whether we're still parsing the overall &&/|| of the vetr
    // expression, or if we've switched to token parsing.

    SEXP fun_symb = CAR(lang);
    // could be pkg::fun, but since base::`&&` is not supported as a token
    // delimiter we can treat that as a normal non-vetr delimiter call.
    if(!token && !is_dot_call && TYPEOF(fun_symb) == SYMSXP) {
      const char * call_symb;
      call_symb = CHAR(PRINTNAME(fun_symb));
      if(!strcmp(call_symb, "&&")) {
        call_type = 1;
      } else if(!strcmp(call_symb, "||")) {
        call_type = 2;
      }
    }
  }
  if(call_type != 1 && call_type != 2) token = 1;
  else if(length(lang) != 3) {
    error(
      "Top-level call to %s should have 2 arguments but has %d:\n\n%s",
      call_type == 1 ? "&&" : "||", length(lang) - 1,
      ALIKEC_deparse_chr(lang, -1, set)
    );
  } else {
    // A vetr expression: pop off the &&/|| symbol and recurse through rest
    lang_iter = CDR(lang_iter);
    lang2_iter = CDR(lang2_iter);
    lang_track_iter = CDR(lang_track_iter);
  }
  // Iterate through each element of LANGSXP (becomes list after 1st pop).

  while(TYPEOF(lang_iter) == LANGSXP || TYPEOF(lang_iter) == LISTSXP) {
    int has_dot_sub = VALC_parse_recurse(
      lang_iter, lang2_iter, lang_track_iter, arg_lang,
      token, set, track_hash, track_hash2, arg_tag
    );
    has_dot = has_dot || has_dot_sub;
    lang_iter = CDR(lang_iter);
    lang2_iter = CDR(lang2_iter);
    lang_track_iter = CDR(lang_track_iter);
  }
  // If this is the first non-(&& or ||) call, and after recursion it was
  // determined to contain `.`, it becomes type 10, otherwise 999.

  if(call_type > 10 && has_dot) call_type = 10;
  else if (!token) has_dot = 0;  // dot doesn't propagate outside token

  if(!token0 && token) {
    // Top level of token, so record the type.
    SETCAR(lang_track_0, ScalarInteger(call_type));
  } else if(!token) {
    // Expression, record the sub-expression types.
    SETCAR(lang_track, ScalarInteger(call_type));
    SETCAR(lang_track_0, lang_track);
  }
  // Re-link our changed language objects back to the calling function versions.
  SETCAR(lang_0, lang);
  SETCAR(lang2_0, lang2);
  // Reset hash levels to avoid spurious collision detection with symbols from
  // outside this parse tree.
  VALC_reset_track_hash(track_hash, substitute_level);
  VALC_reset_track_hash(track_hash2, substitute_level2);

  // counter--; Rprintf(" VV Rec level %d ---------------\n\n", counter);

  UNPROTECT(prt);

  return has_dot;
}
