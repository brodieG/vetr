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
#include "pfhash.h"
#include <time.h>

// - Helper Functions ----------------------------------------------------------

/* equivalent to `mode` in R, note this is a bit approximate and just trying to
hit the obvious corner cases between `typeof` and `mode`*/

const char * ALIKEC_mode_int(SEXP obj) {
  const char * class;
  switch(TYPEOF(obj)) {
    case NILSXP: class = "NULL"; break;
    case SYMSXP: class = "name"; break;
    case FUNSXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case CLOSXP: class = "function"; break;
    case LANGSXP: class = "call"; break;
    case REALSXP: class = "numeric"; break;
    default: class = type2char(TYPEOF(obj));
  }
  return class;
}
SEXP ALIKEC_mode(SEXP obj) {
  return(mkString(ALIKEC_mode_int(obj)));
}
/*
returns specified class, or implicit class if none
*/
SEXP ALIKEC_class(SEXP obj, SEXP class) {
  if(class == R_NilValue) return(ALIKEC_mode(obj));
  return class;
}
/*
run `getOption` from C
*/
SEXP ALIKEC_getopt(const char * opt) {
  SEXP opt_call = PROTECT(list2(ALIKEC_SYM_getOption, mkString(opt)));
  SET_TYPEOF(opt_call, LANGSXP);
  SEXP opt_val = PROTECT(eval(opt_call, R_BaseEnv));
  UNPROTECT(2);
  return opt_val;
}
// - Abstraction ---------------------------------------------------------------
/*
sets the select `tsp` values to zero
*/
SEXP ALIKEC_abstract_ts(SEXP x, SEXP attr) {
  if(TYPEOF(attr) != REALSXP || XLENGTH(attr) != 3) {
    // nocov start
    error("Internal Error: incorrect format for tsp attr, contact maintainer");
    // nocov end
  }
  SEXP x_cp = PROTECT(duplicate(x));

  // Get to last attribute, and make sure tsp is not set

  SEXP attrs = ATTRIB(x_cp), attrs_cpy, attrs_last = R_NilValue;
  for(attrs_cpy = attrs; attrs_cpy != R_NilValue; attrs_cpy = CDR(attrs_cpy)) {
    attrs_last = attrs_cpy;
    if(TAG(attrs_cpy) == R_TspSymbol) break;
  }
  if(attrs_cpy != R_NilValue) {
    // nocov start
    error("Internal Error: object already has a `tsp` attribute");
    // nocov end
  }
  if(attrs_last == R_NilValue) {
    // nocov start
    error("Internal Error: failed finding last attribute when adding tsp");
    // nocov end
  }
  // Illegally append non-kosher tsp attribute

  SETCDR(attrs_last, list1(attr));
  SET_TAG(CDR(attrs_last), R_TspSymbol);
  UNPROTECT(1);
  return x_cp;
}
/*
Run deparse command and return character vector with results

set width_cutoff to be less than zero to use default
*/
SEXP ALIKEC_deparse_core(SEXP obj, int width_cutoff) {
  SEXP quot_call = PROTECT(list2(R_QuoteSymbol, obj));
  SEXP dep_call;

  SET_TYPEOF(quot_call, LANGSXP);

  if(width_cutoff < 0){
    dep_call = PROTECT(list2(ALIKEC_SYM_deparse, quot_call));
  } else {
    dep_call = PROTECT(
      list3(ALIKEC_SYM_deparse, quot_call, ScalarInteger(width_cutoff))
    );
    SET_TAG(CDDR(dep_call), ALIKEC_SYM_widthcutoff);
  }
  SET_TYPEOF(dep_call, LANGSXP);
  SEXP res = eval(dep_call, R_BaseEnv);
  UNPROTECT(2);
  return res;
}
/*
Do a one line deparse, optionally replacing characters in excess of `max_chars`
by `..` to keep deparse short; `keep_at_end` indicates how many characters to
keep at end of deparsed when shortening (e.g. `i_m_deparsed(xyz..)` is keeping
the last parenthesis
*/
const char * ALIKEC_deparse_oneline(
  SEXP obj, size_t max_chars, size_t keep_at_end, struct VALC_settings set
) {
  if(max_chars < 8)
    error("Internal Error: argument `max_chars` must be >= 8");  // nocov
  if(keep_at_end > max_chars - 2)
    error("Internal Error: arg `keep_at_end` too large");  // nocov

  const char * res, * dep_line;
  SEXP dep_line_sexp = PROTECT(ALIKEC_deparse_core(obj, 500));
  dep_line = CHAR(STRING_ELT(dep_line_sexp, 0));
  UNPROTECT(1);

  size_t dep_len = CSR_strmlen(dep_line, set.nchar_max);

  if(dep_len > max_chars) {
    // truncate string and use '..' at the end

    char * res_tmp = R_alloc(dep_len + 1, sizeof(char));
    size_t i, j;
    for(i = 0; i < max_chars - keep_at_end - 2; i++) res_tmp[i] = dep_line[i];
    res_tmp[i] = res_tmp[i + 1] = '.';
    i += 2;
    for(j = dep_len - keep_at_end; j < dep_len && i < dep_len; j++, i++) {
      res_tmp[i] = dep_line[j];
    }
    res_tmp[i] = '\0';
    res = (const char *) res_tmp;
  } else res = dep_line;

  return res;
}
SEXP ALIKEC_deparse_oneline_ext(SEXP obj, SEXP max_chars, SEXP keep_at_end) {
  int char_int = asInteger(max_chars);
  int keep_int = asInteger(keep_at_end);
  if(char_int < 0 || keep_int < 0) {
    // nocov start
    error("Internal Error: arg max_chars and keep_at_end must be positive");
    // nocov end
  }
  struct VALC_settings set = VALC_settings_init();
  return mkString(
    ALIKEC_deparse_oneline(obj, (size_t) char_int, (size_t) keep_int, set)
  );
}
SEXP ALIKEC_deparse(SEXP obj, int width_cutoff) {
  return ALIKEC_deparse_core(obj, width_cutoff);
}
/*
version that uses default deparse width if console is wide enought, otherwise
based on console width
*/
SEXP ALIKEC_deparse_width(SEXP obj, int width) {
  if(width < 10 || width > 1000) width = 80;

  int dep_cutoff;

  if(width < 62) dep_cutoff = width - 2;
  else dep_cutoff = 60;
  if(dep_cutoff < 20) dep_cutoff = 20;
  return ALIKEC_deparse(obj, dep_cutoff);
}
SEXP ALIKEC_deparse_ext(SEXP obj, SEXP width_cutoff) {
  return ALIKEC_deparse(obj, asInteger(width_cutoff));
}
/*
Pad a character vector

@param obj character vector to pad
@param pad how to pad the character vector
  - -1, use the R prompt and continue symbols
  - 0-n pad with that many spaces
@param lines how many lines to show, append `...` a end; set to -1 to ignore
*/
const char * ALIKEC_pad(
  SEXP obj, R_xlen_t lines, int pad, struct VALC_settings set
) {
  if(TYPEOF(obj) != STRSXP)
    error("Internal Error: argument `obj` should be STRSXP");  // nocov
  R_xlen_t line_max = XLENGTH(obj), i;
  if(!line_max) return "";
  for(i = 0; i < line_max; i++)
    if(STRING_ELT(obj, i) == NA_STRING)
      error("Internal Error: argument `obj` contains NAs"); // nocov

  if(lines < 0) lines = line_max;

  char * res = "";
  const char * dep_prompt = "", * dep_continue = "";

  // Figure out what to use as prompt and continue

  if(pad < 0) {
    SEXP prompt_val = PROTECT(ALIKEC_getopt("prompt"));
    SEXP prompt_continue = PROTECT(ALIKEC_getopt("continue"));

    if(
      TYPEOF(prompt_val) != STRSXP || TYPEOF(prompt_continue) != STRSXP ||
      asChar(prompt_val) == NA_STRING || asChar(prompt_continue) == NA_STRING
    ) {
      // nocov start not possible to actually set these as options
      dep_prompt = "> ";
      dep_continue = "+ ";
    } else {
      // nocov end
      dep_prompt = CHAR(asChar(prompt_val));
      dep_continue = CHAR(asChar(prompt_continue));
    }
    UNPROTECT(2);
  } else if (pad > 0) {
    char * pad_chr = R_alloc(pad + 1, sizeof(char));
    int i;
    for(i = 0; i < pad; i++) pad_chr[i] = ' ';
    pad_chr[i] = '\0';
    dep_prompt = dep_continue = (const char *) pad_chr;
  }
  // Cycle through lines

  for(i = 0; i < lines; i++) {
    const char * dep_pad = "";
    const char * dep_err = CHAR(STRING_ELT(obj, i));
    if(!i) dep_pad = dep_prompt; else dep_pad = dep_continue;
    res = CSR_smprintf6(
      set.nchar_max, "%s%s%s%s%s%s", res, dep_pad, dep_err,
      i == lines - 1 && lines < line_max ? "..." : "",
      lines > 1 && line_max > 1 ? "\n" : "", ""
  );}
  return res;
}
SEXP ALIKEC_pad_ext(SEXP obj, SEXP lines, SEXP pad) {
  struct VALC_settings set = VALC_settings_init();
  return mkString(ALIKEC_pad(obj, asInteger(lines), asInteger(pad), set));
}
/*
 * Check whether a language call is an operator call
 */
int ALIKEC_is_an_op(SEXP lang) {
  int is_an_op = 0;
  if(TYPEOF(lang) == LANGSXP) {
    SEXP call = CAR(lang);
    if(TYPEOF(call) == SYMSXP) {
      const char * call_sym = CHAR(PRINTNAME(call));
      int i = 1;
      if(
        !strcmp("+", call_sym) || !strcmp("-", call_sym) ||
        !strcmp("*", call_sym) || !strcmp("/", call_sym) ||
        !strcmp("^", call_sym) || !strcmp("|", call_sym) ||
        !strcmp("||", call_sym) || !strcmp("&", call_sym) ||
        !strcmp("&&", call_sym) || !strcmp("~", call_sym) ||
        !strcmp(":", call_sym) || !strcmp("$", call_sym) ||
        !strcmp("[", call_sym) || !strcmp("[[", call_sym) ||
        !strcmp("!", call_sym) || !strcmp("==", call_sym) ||
        !strcmp("<", call_sym) || !strcmp("<=", call_sym) ||
        !strcmp(">", call_sym) || !strcmp(">=", call_sym)
      ) is_an_op = 1;

      if(!is_an_op && call_sym[0] == '%') {
        // check for %xx% operators
        while(call_sym[i] && i < 1024) i++;
        if(i < 1024 && i > 1 && call_sym[i - 1] == '%') is_an_op = 1;
      }
    }
  }
  return is_an_op;
}
/*
 * Checks whether the innermost part of a call is an OP, in which case if the
 * object that will be inserted in there is an op we probably want to wrap it in
 * parens
 */
int ALIKEC_is_an_op_inner(SEXP lang) {
  SEXP lang_cpy=lang, lang_next;

  // Advance through language object until the first argument is no longer a
  // language object

  while(TYPEOF(lang_next = CADR(lang_cpy)) == LANGSXP) {lang_cpy = lang_next;}

  return ALIKEC_is_an_op(lang_cpy);
}
/*
 * Check whether a language call is to an operator or to other special symbols
 * that are not syntactic but don't require escaping
 */
int ALIKEC_no_esc_needed(SEXP lang) {
  int no_esc = 0;
  if(TYPEOF(lang) == LANGSXP) {
    SEXP call = CAR(lang);
    if(TYPEOF(call) == SYMSXP) {
      const char * call_sym = CHAR(PRINTNAME(call));
      if(!strcmp("(", call_sym) || !strcmp("{", call_sym)) no_esc = 1;
    }
  }
  no_esc += ALIKEC_is_an_op(lang);
  return no_esc;
}

/*
 * Checks whether any names in the language object are non-syntactic and as such
 * should probably not be escaped with backticks.
 *
 * We only recurse through language elements because if we have a non language
 * element that would require recursing (e.g. list) we're pretty much guaranteed
 * the diplay will be more than one line, and at that point we don't care about
 * syntactic or not because we won't be trying to wrap stuff in backticks.
 */

int ALIKEC_syntactic_names(SEXP lang) {
  int syntactic = 1;
  int first = 1;
  SEXP cur_lang;
  if(TYPEOF(lang) == LANGSXP) {
    for(cur_lang = lang; cur_lang != R_NilValue; cur_lang = CDR(cur_lang)) {
      SEXP cur_elem = CAR(cur_lang);
      if(first) {
        // Ok to have an operator call
        first = 0;
        if(ALIKEC_no_esc_needed(cur_lang)) continue;
      }
      syntactic = ALIKEC_syntactic_names(cur_elem);
      if(!syntactic) break;
    }
  } else if (TYPEOF(lang) == SYMSXP) {
    const char * lang_chr = CHAR(PRINTNAME(lang));
    syntactic =
      (lang == R_MissingArg) ||
      ALIKEC_is_keyword(lang_chr) || ALIKEC_is_valid_name(lang_chr);
  }
  return syntactic;
}
SEXP ALIKEC_syntactic_names_exp(SEXP lang) {
  return ScalarLogical(ALIKEC_syntactic_names(lang));
}
/*
 * Deparse a call and quote it, or if it is too long to quote, put on it's own
 * lines and offset otherwise
 *
 * @param lang a language object to deparse and turn into character
 * @param width screen width, use -1 to use `getOption('width')`, this is a
 *   leftover from before the `set` argument was added to this function
 * @param syntactic whether the names in the language object are syntactic or
 *   not.  If there are some that are not, we do not want to quote with
 *   backticks as that gets confusing.  Instead we use braces.  Set to 0 if
 *   there are non-syntactic names, 1 if there are not, and -1 to auto-detect.
 *   Note that this only matters if the language expression deparses to no more
 *   than one line.
 */
struct ALIKEC_pad_quote_res ALIKEC_pad_or_quote(
  SEXP lang, int width, int syntactic, struct VALC_settings set
) {
  switch(syntactic) {
    case -1: syntactic = ALIKEC_syntactic_names(lang); break;
    case 0:
    case 1: break;
      // nocov start
    default: {
      error("Internal Error: unexpected `syntactic` value; contat maintainer");
      // nocov end
    }
  }
  if(width != set.width)
    // nocov start
    error("Internal Error: mismatched width values; contact maintainer.");
    // nocov end

  if(width < 0) width = asInteger(ALIKEC_getopt("width"));
  if(width <= 0 || width == NA_INTEGER) width = 80;
  SEXP lang_dep = PROTECT(ALIKEC_deparse_width(lang, width));

  // Handle the different deparse scenarios

  int multi_line = 1;
  const char * dep_chr = CHAR(asChar(lang_dep));

  if(XLENGTH(lang_dep) == 1) {
    size_t dep_chr_len = CSR_strmlen(dep_chr, set.nchar_max);
    if(width > 2 && dep_chr_len <= (size_t) (width - 2)) multi_line = 0;
  }
  const char * call_char, * call_pre = "", * call_post = "";
  if(multi_line) {
    call_pre = "";
    call_char = ALIKEC_pad(lang_dep, -1, 0, set);
    call_post = "";
  } else {
    // In case there are non syntactic names in the call, use braces instead of
    // backticks to avoid possible confusion; maybe it would better to just scan
    // the deparsed string for backticks?

    if(syntactic) {
      call_pre = "`";
      call_post = "`";
    } else {
      call_pre = "{";
      call_post = "}";
    }
    call_char = dep_chr;
  }
  UNPROTECT(1);
  const char * res = CSR_smprintf4(
    set.nchar_max, "%s%s%s%s", call_pre, call_char, call_post, ""
  );
  return (struct ALIKEC_pad_quote_res) {
    .chr=res, .multi_line=multi_line
  };
}
/*
 * external version for testing
 */
SEXP ALIKEC_pad_or_quote_ext(SEXP lang, SEXP width, SEXP syntactic) {
  struct VALC_settings set = VALC_settings_init();
  set.width = INTEGER(width)[0];
  struct ALIKEC_pad_quote_res res = ALIKEC_pad_or_quote(
    lang, INTEGER(width)[0], INTEGER(syntactic)[0], set
  );
  return mkString(res.chr);
}

/*
deparse into character

@param width_cutoff to use as `width.cutoff` param to `deparse`
@param lines to use as `lines` arg to ALIKEC_pad
*/
const char * ALIKEC_deparse_chr(
  SEXP obj, int width_cutoff, struct VALC_settings set
) {
  SEXP res_dep = PROTECT(ALIKEC_deparse_core(obj, width_cutoff));
  const char * res = ALIKEC_pad(res_dep, -1, 0, set);
  UNPROTECT(1);
  return res;
}

/*
Simplified version of R's internal findFun

Doesn't do quick lookups for special symbols, or use the global cache if it is
available.

Most importantly, instead of failing if function is not found, returns
R_UnboundValue.

The code is copied almost verbatim from src/main/envir.c:findFun()
*/

SEXP ALIKEC_findFun(SEXP symbol, SEXP rho) {
  if(TYPEOF(symbol) != SYMSXP)
    error("Internal Error: `symbol` must be symbol");  // nocov
  if(TYPEOF(rho) != ENVSXP)
    error("Internal Error: `rho` must be environment");// nocov
  SEXP vl;
  while (rho != R_EmptyEnv) {
    vl = findVarInFrame3(rho, symbol, TRUE);
    if (vl != R_UnboundValue) {
      if (TYPEOF(vl) == PROMSXP) {
        PROTECT(vl);
        vl = eval(vl, rho);
        UNPROTECT(1);
      }
      if (
        TYPEOF(vl) == CLOSXP || TYPEOF(vl) == BUILTINSXP ||
        TYPEOF(vl) == SPECIALSXP
      )
        return (vl);
      if (vl == R_MissingArg) {
        return R_UnboundValue;
    } }  // nocov
    rho = ENCLOS(rho);
  }
  return R_UnboundValue;
}
SEXP ALIKEC_findFun_ext(SEXP symbol, SEXP rho) {
  SEXP res = ALIKEC_findFun(symbol, rho);
  if(res == R_UnboundValue) return R_NilValue;
  return res;
}
/*
 * Convert the target and current component strings into one long string
 *
 * Only exists because this operation is expensive and we want to defer carrying
 * out until we're absolutely sure that we need to carry it out.
 */
struct ALIKEC_tar_cur_strings ALIKEC_get_res_strings(
  struct ALIKEC_res_strings strings, struct VALC_settings set
) {
  const char * tar_str = CSR_smprintf4(
    set.nchar_max, strings.target[0], strings.target[1],
    strings.target[2], strings.target[3], strings.target[4]
  );
  const char * cur_str = CSR_smprintf4(
    set.nchar_max, strings.current[0], strings.current[1],
    strings.current[2], strings.current[3], strings.current[4]
  );
  return (struct ALIKEC_tar_cur_strings) {.target=tar_str, .current=cur_str};
}
/*
Convert convention of zero length string == TRUE to SEXP
*/

SEXP ALIKEC_res_as_string(
  struct ALIKEC_res res, SEXP call, struct VALC_settings set
) {
  const char * res_str = "<UNINITSTRING>";
  if(!res.success) {
    struct ALIKEC_tar_cur_strings strings_pasted =
      ALIKEC_get_res_strings(res.dat.strings, set);

    if(TYPEOF(res.wrap) != VECSXP || xlength(res.wrap) != 2) {
      // nocov start
      error(
        "%s%s", "Internal Error: unexpected structure for wrap member; ",
        "contact maintainer."
      );
      // nocov end
    }
    SEXP call_inj = PROTECT(ALIKEC_inject_call(res, call));
    struct ALIKEC_pad_quote_res call_res =
      ALIKEC_pad_or_quote(call_inj, set.width, -1, set);
    const char * call_chr = call_res.chr;
    UNPROTECT(1);

    const char * extra_blank = "";
    if(!call_res.multi_line) extra_blank = " ";
    if(strings_pasted.target[0] && strings_pasted.current[0]) {
      res_str = CSR_smprintf6(
        set.nchar_max,
        "%s%sshould %s %s (%s %s)",
        call_chr, extra_blank, res.dat.strings.tar_pre, strings_pasted.target,
        res.dat.strings.cur_pre, strings_pasted.current
      );
    } else if (res.dat.strings.target[0]) {
      res_str = CSR_smprintf4(
        set.nchar_max, "%s%sshould %s %s", call_chr, extra_blank,
        res.dat.strings.tar_pre, strings_pasted.target
      );
    }
  } else
    error("Internal Error: res_as_string only works with fail res."); // nocov
  return(mkString(res_str));
}
/*
 * variation on ALIKEC_res_as_string that returns the full vector so we can use
 * it with ALIKEC_merge_msg
 */

SEXP ALIKEC_res_as_strsxp(
  struct ALIKEC_res res, SEXP call, struct VALC_settings set
) {
  SEXP res_fin;
  if(!res.success) {
    struct ALIKEC_tar_cur_strings strings_pasted =
      ALIKEC_get_res_strings(res.dat.strings, set);
    SEXP call_inj = PROTECT(ALIKEC_inject_call(res, call));
    struct ALIKEC_pad_quote_res call_res =
      ALIKEC_pad_or_quote(call_inj, set.width, -1, set);
    const char * call_chr = call_res.chr;

    res_fin = PROTECT(allocVector(STRSXP, 5));
    SET_STRING_ELT(res_fin, 0, mkChar(call_chr));
    SET_STRING_ELT(res_fin, 1, mkChar(res.dat.strings.tar_pre));
    SET_STRING_ELT(res_fin, 2, mkChar(strings_pasted.target));
    SET_STRING_ELT(res_fin, 3, mkChar(res.dat.strings.cur_pre));
    SET_STRING_ELT(res_fin, 4, mkChar(strings_pasted.current));
    UNPROTECT(2);
  } else
    error("Internal Error: res_as_strsxp only works with fail res."); // nocov
  return(res_fin);
}
/*
Basic checks that `obj` could be a data frame; does not check class, only that
object is list and that contents are all same length
unfortunately, may not be actually used due to how compare_class is structured
tbd
*/
int ALIKEC_is_dfish(SEXP obj) {
  int res = 1;
  R_xlen_t vec_len, col_num, col_count;
  if(TYPEOF(obj) == VECSXP) {
    col_count = XLENGTH(obj);
    if(col_count) {
      vec_len = XLENGTH(VECTOR_ELT(obj, 0));
      for(col_num = 1; col_num < col_count; col_num++) {
        if(XLENGTH(VECTOR_ELT(obj, col_num)) != vec_len) {
          res = 0;
          break;
    } } }
  } else {
    res = 0;
  }
  return res;
}
SEXP ALIKEC_is_dfish_ext(SEXP obj) {
  return ScalarLogical(ALIKEC_is_dfish(obj));
}
/*
 * Starts with a pair list, and returns it as a VECSXP sorted by tag
 *
 * Note that this uses `qsort` so the sort is not stable on ties.  Our primary
 * use case is to compare attributes where there should not be duplicate tas in
 * the attribute LISTSXP, so that should not matter.
 *
 * Not sure if there is a way to sort a SEXP VECSXP in place, but seems pretty
 * dangerous so we'll settle for the intermediate approach where we sort the
 * indeces and tag names.
 */

struct chr_idx {SEXP name; SEXP val; R_xlen_t idx;};

static int cmpfun (const void * p, const void * q) {
  struct chr_idx a = *(struct chr_idx *) p;
  struct chr_idx b = *(struct chr_idx *) q;
  const char * a_chr = CHAR(a.name);
  const char * b_chr = CHAR(b.name);
  return(strcmp(a_chr, b_chr));
}
SEXP ALIKEC_list_as_sorted_vec(SEXP x) {
  if(x != R_NilValue && TYPEOF(x) != LISTSXP)
    error("Internal Error: input should be NULL or a LISTSXP"); // nocov

  SEXP res, res_nm;

  if(x == R_NilValue) {
    res = PROTECT(PROTECT(allocVector(VECSXP, 0)));
  } else {
    SEXP x_el = x;
    R_xlen_t x_len = xlength(x);

    // Fill our sort buffer and transfer everything to VECSXP

    struct chr_idx * sort_buff =
      (struct chr_idx *) R_alloc((size_t) x_len, sizeof(struct chr_idx));

    for(R_xlen_t i = 0; i < x_len; ++i) {
      SEXP nm = TAG(x_el) == R_NilValue ? R_BlankString : PRINTNAME(TAG(x_el));
      *(sort_buff + i) = (struct chr_idx) {
        .name = nm, .val = CAR(x_el), .idx = i
      };
      x_el = CDR(x_el);
    }
    // Sort the buffer and reorder the vectors

    qsort(sort_buff, (size_t) x_len, sizeof(struct chr_idx), cmpfun);

    // `head` holds the data at the current spot that needs to be overwritten

    res = PROTECT(allocVector(VECSXP, x_len));
    res_nm = PROTECT(allocVector(STRSXP, x_len));

    for(R_xlen_t i = 0; i < x_len; ++i) {
      struct chr_idx tar = *(sort_buff + i);
      SET_VECTOR_ELT(res, i, tar.val);
      SET_STRING_ELT(res_nm, i, tar.name);
    }
    setAttrib(res, R_NamesSymbol, res_nm);
  }
  UNPROTECT(2);
  return res;
}
