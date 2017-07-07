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
#include <stdint.h>

/*
 * Initialize settings with default values
 */
struct VALC_settings VALC_settings_init() {
  return (struct VALC_settings) {
    .type_mode = 0,
    .attr_mode = 0,
    .lang_mode = 0,
    .fun_mode = 0,
    .fuzzy_int_max_len = 100,
    .suppress_warnings = 0,
    .in_attr = 0,
    .env = R_NilValue,
    .width = -1,
    .env_depth_max = 65535L,
    .symb_sub_depth_max = 65535L,
    .nchar_max = 65535L,
    .symb_size_max = 15000L,
    .track_hash_content_size = 63L
  };
}
/*
 * Check that a SEXP could pass as a scalar integer and return it as a long
 */
static long VALC_is_scalar_int(
  SEXP x, const char * x_name, int x_min, int x_max
) {
  SEXPTYPE x_type = TYPEOF(x);

  if(x_type != REALSXP && x_type != INTSXP)
    error(
      "Setting `%s` must be integer-like (is %s).", x_name, type2char(x_type)
    );

  // Despite L notation, R integers are just ints, but there are checks to
  // ensure ints are 32 bits on compilation and such

  int x_int = asInteger(x);

  if(xlength(x) != 1)
    error(
      "Setting `%s` must be scalar integer (is length %zu).", x_name,
      xlength(x)
  );
  if(x_int == NA_INTEGER) error("Setting `%s` may not be NA.", x_name);
  if(TYPEOF(x) == REALSXP) {
    if(x_int != asReal(x)) error("Setting `%s` must be integer like.", x_name);
  }
  if(x_int < x_min || x_int > x_max)
    error(
      "Setting `%s` must be scalar integer between %d and %d (is %d).",
      x_name, x_min, x_max, x_int
    );
  return x_int;
}
/*
 * Convert input setting list into settings structure, validating
 * along the way
 *
 * Not ideal that we have defaults defined both in `vet_settings` and here, but
 * it is fastest this way
 */

struct VALC_settings VALC_settings_vet(SEXP set_list, SEXP env) {
  struct VALC_settings settings = VALC_settings_init();
  R_xlen_t set_len = 14;

  if(TYPEOF(set_list) == VECSXP) {
    if(xlength(set_list) != set_len) {
      error(
        "`vet/vetr` usage error: `settings` must be a list of length %zu.",
        set_len
      );
    }
    SEXP set_names = getAttrib(set_list, R_NamesSymbol);
    if(set_names == R_NilValue || TYPEOF(set_names) != STRSXP) {
      error(
        "%s%s%s", "`vet/vetr` usage error: ",
        "argument `settings` must be a named list as produced ",
        "by `vetr_settings`."
      );
    }
    const char * set_names_default[] = {
      "type.mode", "attr.mode", "lang.mode", "fun.mode", "rec.mode",
      "suppress.warnings", "fuzzy.int.max.len",
      "width", "env.depth.max", "symb.sub.depth.max", "symb.size.max",
      "nchar.max", "track.hash.content.size", "env"
    };
    SEXP set_names_def_sxp = PROTECT(allocVector(STRSXP, set_len));
    for(R_xlen_t i = 0; i < set_len; ++i) {
      SET_STRING_ELT(set_names_def_sxp, i, mkChar(set_names_default[i]));
    }
    if(!R_compute_identical(set_names, set_names_def_sxp, 16)) {
      error(
        "%s%s",
        "`vet/vetr` usage error: argument `settings` names are not in format  ",
        "produced by `vetr_settings`."
      );
    }
    set_names_def_sxp = R_NilValue;
    UNPROTECT(1);
    // check the scalar integers

    settings.type_mode =
      VALC_is_scalar_int(VECTOR_ELT(set_list, 0), "type.mode", 0, 2);
    settings.attr_mode =
      VALC_is_scalar_int(VECTOR_ELT(set_list, 1), "attr.mode", 0, 2);
    settings.lang_mode =
      VALC_is_scalar_int(VECTOR_ELT(set_list, 2), "lang.mode", 0, 2);
    settings.fun_mode =
      VALC_is_scalar_int(VECTOR_ELT(set_list, 3), "fun.mode", 0, 2);
    settings.rec_mode =
      VALC_is_scalar_int(VECTOR_ELT(set_list, 4), "rec.mode", 0, 2);
    settings.fuzzy_int_max_len = VALC_is_scalar_int(
      VECTOR_ELT(set_list, 6), "fuzzy.int.max.len", INT_MIN, INT_MAX
    );
    settings.width =
      VALC_is_scalar_int(VECTOR_ELT(set_list, 7), "width", -1, INT_MAX);
    settings.env_depth_max =
      VALC_is_scalar_int(VECTOR_ELT(set_list, 8), "env.depth.max", -1, INT_MAX);
    settings.symb_sub_depth_max = VALC_is_scalar_int(
      VECTOR_ELT(set_list, 9), "symb.sub.depth.max", 0, INT_MAX
    );
    settings.nchar_max =
      VALC_is_scalar_int(VECTOR_ELT(set_list, 10), "nchar.max", 0, INT_MAX);
    settings.symb_size_max = VALC_is_scalar_int(
      VECTOR_ELT(set_list, 11), "symb.size.max", 0, INT_MAX
    );
    settings.track_hash_content_size = VALC_is_scalar_int(
      VECTOR_ELT(set_list, 12), "track.hash.content.size", 0, INT_MAX
    );
    // Other checks

    SEXP sup_warn = VECTOR_ELT(set_list, 5);
    if(
      TYPEOF(sup_warn) != LGLSXP || xlength(sup_warn) != 1 ||
      asInteger(sup_warn) == NA_LOGICAL
    ) {
      error(
        "%s%s",
        "`vet/vetr` usage error: setting `suppress.warnings` must be TRUE ",
        "or FALSE"
      );
    }
    settings.suppress_warnings = asLogical(sup_warn);

    if(
      TYPEOF(VECTOR_ELT(set_list, 13)) != ENVSXP &&
      VECTOR_ELT(set_list, 13) != R_NilValue
    ) {
      error(
        "%s%s",
        "`ver/vetr` usage error: setting `env` must be an environment ",
        "or NULL"
      );
    }
    settings.env = VECTOR_ELT(set_list, 13);
  } else if (set_list != R_NilValue) {
    error(
      "%s (is %s).",
      "`vet/vetr` usage error: argument `settings` must be a list or NULL",
      type2char(TYPEOF(set_list))
    );
  }
  if(TYPEOF(env) != ENVSXP) {
    error("`vet/vetr` usage error: argument `env` must be an environment.");
  }
  if(settings.env == R_NilValue) settings.env = env;
  return settings;
}
