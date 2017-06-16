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
    .env_depth_max = 65535L,
    .symb_sub_depth_max = 65535L,
    .nchar_max = 65535L;
    .symb_size_max = 15000L;
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
  struct VALC_settings settings = VALC_settings_init();

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
      "width", "env.depth.max", "symb.sub.depth.max", "nchar.max",
      "track.hash.content.size"
    }
    SEXP set_names_def_sxp = PROTECT(allocVector(STRSXP, set_len));
    for(R_xlen_t i = 0; i < set_len; ++i) {
      SET_VECTOR_ELT(set_names_def_sxp, i, mkChar(*settings_names[i]));
    }
    if(!R_compute_identical(set_names, set_names_def_sxp, 16)) {
      error(
      "Argument `settings` names are not in format produced by ",
      "`vetr_settings`."
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
      VECTOR_ELT(set_list, 6), "fuzzy.int.max.len", 0L, LONG_MAX
    );
    settings.width =
      VALC_is_scalar_int(VECTOR_ELT(set_list, 7), "width", -1L, LONG_MAX);
    settings.env_depth_max =
      VALC_is_scalar_int(VECTOR_ELT(set_list, 8), "env.depth.max", 0, LONG_MAX);
    settings.symb_depth_max = VALC_is_scalar_int(
      VECTOR_ELT(set_list, 9), "symb.depth.max", 0, LONG_MAX
    );
    settings.nchar_max =
      VALC_is_scalar_int(VECTOR_ELT(set_list, 10), "nchar.max", 0, LONG_MAX);
    settings.symb_size_max =
      VALC_is_scalar_int(VECTOR_ELT(set_list, 11), "symb.size.max", 0, LONG_MAX);
    settings.track_hash_content_size = VALC_is_scalar_int(
      VECTOR_ELT(set_list, 12), "track.hash.content.size", 0, LONG_MAX
    );

    // Other checks

    SEXP sup_warn = VECTOR_ELT(set_list, 5);
    if(
      TYPEOF(sup_warn) != LGLSXP || xlength(sup_warn) != 1 ||
      sup_warn == NA_LOGICAL
    ) {
      error("Setting `suppress.warnings` must be TRUE or FALSE");
    }
  }
  return settings;
}
