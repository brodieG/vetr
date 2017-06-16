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
