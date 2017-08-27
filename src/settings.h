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

#include <Rinternals.h>

#ifndef _VETR_SET_H
#define _VETR_SET_H

  // QUESTION, WHAT TYPE SHOULD ALL THE NUMBERS HERE BE, LONG? THAT WOULD SEEM
  // TO MAKE SENSE

  struct VALC_settings {
    // Original alike settings

    int type_mode, attr_mode, lang_mode, fun_mode, rec_mode;

    // Length of numeric vectors to consider for integer-likeness

    int fuzzy_int_max_len;

    int suppress_warnings;

    // internal, track whether we are recursing through attributes

    int in_attr;

    int width;      // Tell alike what screen width to assume

    // what env to look for functions to match call in, substitute, etc, used
    // both by alike and by vet funs

    SEXP env;

    // Additional vet settings

    size_t env_depth_max; // how many envs to track when searching for env loop
    size_t nchar_max;     // when do we stop looking for NULL?
    size_t symb_sub_depth_max;   // how deep recursive substitution can go?
    size_t symb_size_max;
    size_t track_hash_content_size;

    int result_list_size_init;
    int result_list_size_max;
  };
  struct VALC_settings VALC_settings_init();
  struct VALC_settings VALC_settings_vet(SEXP set_list, SEXP env);

#endif
