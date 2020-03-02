/*
Copyright (C) 2020 Brodie Gaslam

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

  // Our result holds the C and R data in separate structures mostly because it
  // would be slow to translate the C stuff into R so we defer that until we're
  // actually positive we have to make the conversion (i.e. all branches of OR
  // statements fail).  We are quite wastefull of space here, but it is easier
  // this way and there shouldn't be that many objects

  struct VALC_res_dat {
    struct ALIKEC_res_dat tpl_dat;   // Data from template token

    // The SEXP data, which can either be:
    //
    // * wrap data from template token, or:
    // * Standard token result, a 2 long VECSXP with the standard token
    //   language in position 0, and the result of evaluating it in position 1

    SEXP sxp_dat;
  };
  // Holds all the template or standard token data

  struct VALC_res {
    struct VALC_res_dat dat;
    int tpl;          // template or standard token res?
    int success;
  };
  // Holds all the template or standard token data, except for the SEXP data
  // which is kept separately.  This is intended explicitly as a member of the
  // VALC_res_list array.

  struct VALC_res_node {
    struct ALIKEC_res_dat tpl_dat;
    int tpl;          // template or standard token res?
    int success;
  };
  // Used to track the results of multiple tokens

  struct VALC_res_list {
    struct VALC_res_node * list_tpl;
    SEXP list_sxp;      // this is a pairlist
    SEXP list_sxp_tail; // end of pairlist

    // index of free slot (and count of how many we have), note this means that
    // the last recorded result is at .list[.idx - 1], not .list[.idx]
    int idx;
    int idx_alloc;    // how many we've allocated memory for
    int idx_alloc_max;// max we are allowed to allocate
  };

  extern SEXP VALC_SYM_one_dot;
  extern SEXP VALC_SYM_deparse;
  extern SEXP VALC_SYM_paren;
  extern SEXP VALC_SYM_quote;
  extern SEXP VALC_SYM_current;
  extern SEXP VALC_TRUE;
  extern SEXP VALC_SYM_errmsg;

  SEXP VALC_test1(SEXP a);
  SEXP VALC_test2(SEXP a, SEXP b);
  SEXP VALC_test3(SEXP a, SEXP b, SEXP c);

  SEXP VALC_check_assumptions();

  SEXP VALC_res_init();
  struct VALC_res_list VALC_res_add(
    struct VALC_res_list list, struct VALC_res res
  );
  struct VALC_res_list VALC_res_list_init(struct VALC_settings set);

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
    SEXP lang, SEXP var_name, struct VALC_settings settings, SEXP arg_tag
  );
  SEXP VALC_parse_ext(SEXP lang, SEXP var_name, SEXP rho);
  void VALC_parse_recurse(
    SEXP lang, SEXP lang2, SEXP lang_track, SEXP var_name, int eval_as_is,
    SEXP first_fun, struct VALC_settings set,
    struct track_hash * track_hash, struct track_hash * track_hash2,
    SEXP arg_tag
  );
  SEXP VALC_sub_symbol(
    SEXP lang, struct VALC_settings set, struct track_hash * track_hash,
    SEXP arg_tag
  );
  SEXP VALC_sub_symbol_ext(SEXP lang, SEXP rho);
  void VALC_install_objs();
  SEXP VALC_evaluate(
    SEXP lang, SEXP arg_lang, SEXP arg_tag, SEXP arg_value, SEXP lang_full,
    struct VALC_settings set, int use_lang_raw
  );
  SEXP VALC_evaluate_ext(
    SEXP lang, SEXP arg_lang, SEXP arg_tag, SEXP arg_value, SEXP lang_full,
    SEXP rho
  );
  void VALC_arg_error(SEXP tag, SEXP fun_call, const char * err_base);
  void psh(const char * lab);
#endif
