/*
 * Initialize settings with default values
 */
struct VALC_settings VALC_settings_init() {
  (struct VALC_settings) {
    .type_mode = 0,
    .attr_mode = 0,
    .lang_mode = 0,
    .fun_mode = 0,
    .fuzzy_int_max_len = 0,
    .suppress_warnings = 0,
    .in_attr = 0,
    .width = 0,
    .env_limit = 65535L,
    .max_nchar = 65535L,
    .max_sub_depth = 65535L;
    .max_symbol_size = 15000L;
    .track_hash_content_size = 63L;
  };
}
/*
 * Check that a SEXP could pass as a scalar integer and return it as a long
 */
static long VALC_is_scalar_int <- function(
  SEXP x, const char * x_name, long x_min, long x_max
) {
  SEXPTYPE x_type = TYPEOF(x);
  long x_int = asInteger(x);

  if(xlength(x) != 1) error("Setting `%s` must be scalar integer.", x_name);
  if(x_int == NA_INTEGER) error("Setting `%s` may not be NA.", x_name);
  if(TYPEOF(x) == REALSXP) {
    if(x_int != asReal(x)) error("Setting `%s` must be integer like.", x_name)
  }
  if(x_int < x_min || x_int > x_max)
    error(
      "Setting `%s` must be scalar integer between %d and %d.",
      x_name, x_min, x_max
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

struct VALC_settings VALC_settings_vet(SEXP set_list) {
  struct VALC_settings = VALC_settings_init();

  if(TYPEOF(set_list) == VECSXP) {
    if(xlength(set_list) != R_xlen_t) {
      error("Argument `settings` must be a list of length %zu.", set_len);
    }
    SEXP set_names = getAttrib(set_list, R_NamesSymbol);
    if(set_names == R_NilValue || TYPEOF(names) != STRSXP) {
      error(
        "%s%s", "Argument `settings` must be a named list as produced ",
        "by `vetr_settings`."
      )
    }
    R_xlen_t set_len = 11;
    const char * set_names_default[] = {
      "type.mode", "attr.mode", "lang.mode", "fun.mode", "rec.mode",
      "suppress.warnings", "fuzzy.int.max.len",
      "width", "env.depth.max", "symb.sub.depth.max", "nchar.max"
    }
    SEXP set_names_def_sxp = PROTECT(allocVector(STRSXP, set_len));
    for(R_xlen_t i = 0; i < set_len; ++i) {
      SET_VECTOR_ELT(set_names_def_sxp, i, mkChar(*settings_names[i]));
    }
    if(!R_compute_identical(set_names, set_names_def_sxp, 16)) {
      error(
      "A;rgument `settings` names are not in format produced by ",
      "`vetr_settings`."
      );
    }


    VALC_settings
    SEXPTYPE int_mod_type, fuzzy_type, attr_mod_type, lang_mod_type, width_type,
      env_limit_type;
    int supp_warn = 0, type_int = 0, attr_int = 0, lang_int = 0, width_int = -1,
      env_limit_int;
    R_xlen_t fuzzy_int_max_len_int;

    int_mod_type = TYPEOF(type_mode);
    attr_mod_type = TYPEOF(attr_mode);
    lang_mod_type = TYPEOF(lang_mode);
    fuzzy_type = TYPEOF(fuzzy_int_max_len);
    width_type = TYPEOF(width);
    env_limit_type = TYPEOF(env_limit);

    if(
      (int_mod_type != INTSXP && int_mod_type != REALSXP) ||
      XLENGTH(type_mode) != 1 || (type_int = asInteger(type_mode)) == NA_INTEGER
      || type_int < 0 || type_int > 2
    )
      error("Argument `type.mode` must be a one length numeric between 0 and 2");
    if(
      (attr_mod_type != INTSXP && attr_mod_type != REALSXP) ||
      XLENGTH(attr_mode) != 1 || (attr_int = asInteger(attr_mode)) == NA_INTEGER ||
      attr_int < 0 || attr_int > 2
    )
      error("Argument `attr.mode` must be a one length numeric");
    if(
      (lang_mod_type != INTSXP && lang_mod_type != REALSXP) ||
      XLENGTH(lang_mode) != 1 || (lang_int = asInteger(lang_mode)) == NA_INTEGER ||
      lang_int < 0 || lang_int > 1
    )
      error("Argument `lang.mode` must be a one length numeric between 0 and 1");
    if(
      (fuzzy_type != INTSXP && fuzzy_type != REALSXP) ||
      XLENGTH(fuzzy_int_max_len) != 1 ||
      (fuzzy_int_max_len_int = asInteger(fuzzy_int_max_len)) == NA_INTEGER
    )
      error("Argument `fuzzy.int.max.len` must be an integer one length vector");
    if(
      TYPEOF(suppress_warnings) != LGLSXP || XLENGTH(suppress_warnings) != 1 ||
      (supp_warn = asLogical(suppress_warnings)) == NA_LOGICAL
    )
      error("Argument `suppress.warnings` must be TRUE or FALSE");
    if(env != R_NilValue && TYPEOF(env) != ENVSXP)
      error("Argument `env` must be NULL or an environment");
    if(
      (width_type != INTSXP && width_type != REALSXP) ||
      XLENGTH(width) != 1 || (width_int = asInteger(width)) == NA_INTEGER
    )
      error("Argument `width` must be an integer one length vector");
    if(
      (env_limit_type != INTSXP && env_limit_type != REALSXP) ||
      XLENGTH(env_limit) != 1 ||
      (env_limit_int = asInteger(env_limit)) == NA_INTEGER ||
      env_limit_int < 1
    )
      error(
        "%s%s",
        "Argument `env.limit` must be a strictly positive ",
        "an integer one length vector"
      );

  } else if (set_list != R_NilValue) {
    error(
      "%s%s",
      "Argument `settings` must be a list generated by `vetr_settings`, or ",
      "NULL."
    )
  }



}
