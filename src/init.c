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

#include "validate.h"
#include "all-bw.h"
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
  {"validate", (DL_FUNC) &VALC_validate, 8},
  {"validate_args", (DL_FUNC) &VALC_validate_args, 5},
  {"name_sub", (DL_FUNC) &VALC_name_sub_ext, 2},
  {"symb_sub", (DL_FUNC) &VALC_sub_symbol_ext, 2},
  {"parse", (DL_FUNC) &VALC_parse_ext, 3},
  {"remove_parens", (DL_FUNC) &VALC_remove_parens, 1},
  {"eval_check", (DL_FUNC) &VALC_evaluate_ext, 6},
  {"all", (DL_FUNC) &VALC_all_ext, 1},
  {"track_hash", (DL_FUNC) &VALC_track_hash_test, 2},
  {"default_hash_fun", (DL_FUNC) &VALC_default_hash_fun, 1},
  {"all_bw", (DL_FUNC) &VALC_all_bw, 5},
  {"check_assumptions", (DL_FUNC) &VALC_check_assumptions, 0},

/*
  {"test1", (DL_FUNC) &VALC_test1, 1},
  {"test2", (DL_FUNC) &VALC_test2, 2},
  {"test3", (DL_FUNC) &VALC_test3, 3},
*/
  {"alike_ext", (DL_FUNC) &ALIKEC_alike_ext, 5},
  {"typeof", (DL_FUNC) &ALIKEC_typeof, 1},
  {"mode", (DL_FUNC) &ALIKEC_mode, 1},
  {"type_alike", (DL_FUNC) &ALIKEC_type_alike, 4},
  {"syntactic_names", (DL_FUNC) &ALIKEC_syntactic_names_exp, 1},
  {"compare_attributes", (DL_FUNC) &ALIKEC_compare_attributes, 3},
  {"is_valid_name_ext", (DL_FUNC) &ALIKEC_is_valid_name_ext, 1},
  {"is_dfish", (DL_FUNC) &ALIKEC_is_dfish_ext, 1},
  {"compare_names", (DL_FUNC) &ALIKEC_compare_special_char_attrs, 2},
  {"compare_dimnames", (DL_FUNC) &ALIKEC_compare_dimnames_ext, 2},
  {"compare_class", (DL_FUNC) &ALIKEC_compare_class_ext, 2},
  {"compare_dims", (DL_FUNC) &ALIKEC_compare_dim_ext, 5},
  {"compare_ts", (DL_FUNC) &ALIKEC_compare_ts_ext, 2},
  {"lang_alike", (DL_FUNC) &ALIKEC_lang_alike_ext, 3},
  {"lang_alike_chr", (DL_FUNC) &ALIKEC_lang_alike_chr_ext, 3},
  {"fun_alike", (DL_FUNC) &ALIKEC_fun_alike_ext, 2},
  {"deparse", (DL_FUNC) &ALIKEC_deparse_ext, 2},
  {"deparse_oneline", (DL_FUNC) &ALIKEC_deparse_oneline_ext, 3},
  {"pad", (DL_FUNC) &ALIKEC_pad_ext, 3},
  {"pad_or_quote", (DL_FUNC) &ALIKEC_pad_or_quote_ext, 3},
  {"match_call", (DL_FUNC) &ALIKEC_match_call, 3},
  {"abstract_ts", (DL_FUNC) &ALIKEC_abstract_ts, 2},
  {"env_track", (DL_FUNC) &ALIKEC_env_track_test, 3},
  {"msg_sort", (DL_FUNC) &ALIKEC_sort_msg_ext, 1},
  {"msg_merge", (DL_FUNC) &ALIKEC_merge_msg_ext, 1},
  {"msg_merge_2", (DL_FUNC) &ALIKEC_merge_msg_2_ext, 1},
  {"hash_test", (DL_FUNC) &pfHashTest, 2},
  {"hash_test2", (DL_FUNC) &pfHashTest2, 2},
  {"find_fun", (DL_FUNC) &ALIKEC_findFun_ext, 2},
  {"list_as_sorted_vec", (DL_FUNC) &ALIKEC_list_as_sorted_vec, 1},

  {"len_chr_len_ext", (DL_FUNC) &CSR_len_chr_len_ext, 1},
  {"len_as_chr_ext", (DL_FUNC) &CSR_len_as_chr_ext, 1},
  {"num_as_chr", (DL_FUNC) &CSR_num_as_chr_ext, 2},
  {"strmlen_ext", (DL_FUNC) &CSR_strmlen_ext, 2},
  {"strmcpy_ext", (DL_FUNC) &CSR_strmcpy_ext, 2},
  {"collapse_ext", (DL_FUNC) &CSR_collapse_ext, 3},
  {"bullet_ext", (DL_FUNC) &CSR_bullet_ext, 4},
  {"strsub", (DL_FUNC) &CSR_strsub, 3},
  {"nchar_u", (DL_FUNC) &CSR_nchar_u, 1},
  {"char_offsets", (DL_FUNC) &CSR_char_offsets, 1},
  {"smprintf2_ext", (DL_FUNC) &CSR_smprintf2_ext, 4},
  {"smprintf6_ext", (DL_FUNC) &CSR_smprintf6_ext, 8},
  {"ucfirst_ext", (DL_FUNC) &CSR_ucfirst_ext, 2},
  {"lcfirst_ext", (DL_FUNC) &CSR_lcfirst_ext, 2},
  {"test_strmcpy", (DL_FUNC) &CSR_test_strmcpy, 0},
  {"test_strappend", (DL_FUNC) &CSR_test_strappend, 0},
  {"test_add_szt", (DL_FUNC) &CSR_test_add_szt, 0},
  {"test_smprintfx", (DL_FUNC) &CSR_test_smprintfx, 0},
  {"test_strappend2", (DL_FUNC) &CSR_test_strappend2, 0},

  {NULL, NULL, 0}
};

void R_init_vetr(DllInfo *info)
{
 /* Register the .C and .Call routines.
    No .Fortran() or .External() routines,
    so pass those arrays as NULL.
  */
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, FALSE);
  VALC_SYM_quote = install("quote");
  VALC_SYM_deparse = install("deparse");
  VALC_SYM_one_dot = install(".");
  VALC_SYM_paren = install("(");
  VALC_SYM_current = install("current");
  VALC_SYM_errmsg = install("err.msg");
  VALC_TRUE = ScalarLogical(1);

  // Some overlap with previous since these used to be separate packages...

  ALIKEC_SYM_package = install("package");
  ALIKEC_SYM_inherits = install("inherits");
  ALIKEC_SYM_paren_open = install("(");
  ALIKEC_SYM_tilde = install("~");
  ALIKEC_SYM_args = install("args");
  ALIKEC_SYM_deparse = install("deparse");
  ALIKEC_SYM_nlines = install("nlines");
  ALIKEC_SYM_widthcutoff = install("width.cutoff");
  ALIKEC_SYM_getOption = install("getOption");
  ALIKEC_SYM_matchcall = install("match.call");
  ALIKEC_SYM_current = install("current");
  ALIKEC_SYM_attributes = install("attributes");
  ALIKEC_SYM_attr = install("attr");
  ALIKEC_SYM_colnames = install("colnames");
  ALIKEC_SYM_length = install("length");
  ALIKEC_SYM_syntacticnames = install("syntacticnames");
}

