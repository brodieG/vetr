#include <Rinternals.h>

#ifndef _VETR_SET_H
#define _VETR_SET_H

  // QUESTION, WHAT TYPE SHOULD ALL THE NUMBERS HERE BE, LONG? THAT WOULD SEEM
  // TO MAKE SENSE

  struct VALC_settings {

    // Original alike settings

    int type_mode, attr_mode, lang_mode, fun_mode;
    int fuzzy_int_max_len;
    int suppress_warnings;
    const char * prepend;     // no longer in use

    int in_attr;
    int width;      // Tell alike what screen width to assume
    int env_limit;  // how many envs to track when searching for env loop

    // what env to look for functions to match call in, substitute, etc, used
    // boty by alike and by vet funs

    SEXP env;

    // Additional vet settings

    long max_nchar;       // when do we stop looking for NULL?
    long max_sub_depth;   // how deep do we allow recursive substitution to go?
    long max_symbol_size;
    long track_hash_content_size;
  };
  struct VALC_settings VALC_settings_init();

#endif
