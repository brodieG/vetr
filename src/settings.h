#include <Rinternals.h>

#ifndef _VETR_SET_H
#define _VETR_SET_H

  // QUESTION, WHAT TYPE SHOULD ALL THE NUMBERS HERE BE, LONG? THAT WOULD SEEM
  // TO MAKE SENSE

  struct VALC_settings {
    // Original alike settings

    int type_mode, attr_mode, lang_mode, fun_mode;

    size_t fuzzy_int_max_len;

    int suppress_warnings;
    int in_attr;

    long width;      // Tell alike what screen width to assume

    // what env to look for functions to match call in, substitute, etc, used
    // boty by alike and by vet funs

    SEXP env;

    // Additional vet settings

    size_t env_depth_max; // how many envs to track when searching for env loop
    size_t nchar_max;     // when do we stop looking for NULL?
    size_t symb_sub_depth_max;   // how deep recursive substitution can go?
    size_t symb_size_max;
    size_t track_hash_content_size;
  };
  struct VALC_settings VALC_settings_init();
  struct VALC_settings VALC_settings_vet(SEXP set_list);

#endif
