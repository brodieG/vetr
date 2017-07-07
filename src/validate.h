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

#include <R.h>
#include <Rinternals.h>
#include <ctype.h>
#include "trackinghash.h"
#include "alike.h"

// - Objects We Install Once ---------------------------------------------------

// One question: are static objects not garbage collected?  The examples from
// "Writing R Extensions" don't seem to require the protection of these

#ifndef _VETR_H
#define _VETR_H

  SEXP VALC_SYM_one_dot;
  SEXP VALC_SYM_deparse;
  SEXP VALC_SYM_paren;
  SEXP VALC_SYM_quote;
  SEXP VALC_SYM_current;
  SEXP VALC_TRUE;
  SEXP VALC_SYM_errmsg;

  SEXP VALC_validate(
    SEXP target, SEXP current, SEXP cur_sub, SEXP par_call, SEXP rho,
    SEXP ret_mode_sxp, SEXP stop, SEXP settings
  );
  SEXP VALC_validate_args(
    SEXP fun, SEXP fun_call, SEXP val_call, SEXP fun_frame, SEXP settings
  );
  SEXP VALC_remove_parens(SEXP lang);
  SEXP VALC_name_sub_ext(SEXP symb, SEXP arg_name);
  void VALC_stop(SEXP call, const char * msg);
  void VALC_stop2(SEXP call, const char * msg, SEXP rho);
  SEXP VALC_all_ext(SEXP vec);
  int VALC_all(SEXP vec);
  int IS_TRUE(SEXP x);
  int IS_LANG(SEXP x);
  SEXP VALC_parse(
    SEXP lang, SEXP var_name, struct VALC_settings settings
  );
  SEXP VALC_parse_ext(SEXP lang, SEXP var_name, SEXP rho);
  void VALC_parse_recurse(
    SEXP lang, SEXP lang_track, SEXP var_name, int eval_as_is,
    SEXP first_fun, struct VALC_settings set, struct track_hash * track_hash
  );
  SEXP VALC_sub_symbol(
    SEXP lang, struct VALC_settings set, struct track_hash * track_hash
  );
  SEXP VALC_sub_symbol_ext(SEXP lang, SEXP rho);
  void VALC_install_objs();
  SEXP VALC_evaluate(
    SEXP lang, SEXP arg_lang, SEXP arg_tag, SEXP arg_value, SEXP lang_full,
    struct VALC_settings set
  );
  SEXP VALC_evaluate_ext(
    SEXP lang, SEXP arg_lang, SEXP arg_tag, SEXP arg_value, SEXP lang_full,
    SEXP rho
  );
  void VALC_arg_error(SEXP tag, SEXP fun_call, const char * err_base);
  void psh(const char * lab);

#endif
