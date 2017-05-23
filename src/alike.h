#include "alike.h"
#include "pfhash.h"
#include <wchar.h>

#ifndef _ALIKEC_H
#define _ALIKEC_H

  // - Constants --------------------------------------------------------------

  #define ALIKEC_MAX_CHAR 50000
  #define ALIKEC_MAX_ENVS 65536

  // - Data Structures ---------------------------------------------------------

  /*
  index structures used to track the location at which an error occurs
  */
  union ALIKEC_index_raw {
    R_xlen_t num;
    const char * chr;
  };
  struct ALIKEC_index {
    union ALIKEC_index_raw ind;
    int type;               // 0 is numeric, 1 is character
  };
  struct ALIKEC_res_fin {
    const char * tar_pre;
    const char * target;
    const char * act_pre;
    const char * actual;
    const char * call;
  };
  // Keep track of environments in recursion to make sure we don't get into a
  // infinite recursion loop

  struct ALIKEC_env_track {
    int stack_size;
    int stack_ind;
    int stack_mult;
    int stack_size_init;
    int no_rec;       // prevent further recursion into environments
    SEXP * env_stack;
    int debug;
  };
  // track indices of error, this will be allocated with as many items as
  // there are recursion levels.

  struct ALIKEC_rec_track {
    size_t lvl;        // recursion depth
    size_t lvl_max;    // max recursion depth so far
    struct ALIKEC_index * indices;
    struct ALIKEC_env_track * envs;
    int gp;            // general purpose flag
  };

  // Intermediate structure that will eventually be made part of the `message`
  // component of ALIKEC_res

  struct ALIKEC_res_strings {
    const char * tar_pre;
    const char * target;
    const char * act_pre;
    const char * actual;
  };
  // We used a SEXP because it contains the error message, as well as the wrap
  // component that we can use around the call (e.g "names(%s)[[1]]"), and the
  // latter contains symbols
  //
  // The SEXP is of type VECSXP (i.e. list), and contains two elements.
  //
  // The first element is the message, which itself contains the "target"
  // string, i.e. what the object should be, and the "actual", what it is.,
  //
  // The second is the wrap which is a two element list where the first element
  // is the wrapping call, and the second (I think) is a pointer to the inside
  // of the call which is where we will ultimately substitute the original call
  // (not 100% certain of this; I'm writing these docs way after the fact...)

  struct ALIKEC_res {
    int success;
    SEXP message;
    int df;
    struct ALIKEC_rec_track rec;
  };
  /*
  Results for language recursion comparisions
  */
  struct ALIKEC_res_lang {
    int success;
    struct ALIKEC_rec_track rec;
    struct ALIKEC_res_strings msg_strings;
  };
  // Structure used for functions called by 'alike_obj', main difference with
  // the return value of 'alike_obj' is 'indices', since that is a more complex
  // object that requires initialization

  struct ALIKEC_res_sub {
    int success;
    SEXP message;
    int df;      // whether df or not, not use by all functions
    int lvl;     // Type of error used for prioritizing
  };
  struct ALIKEC_settings {
    int type_mode, attr_mode, lang_mode, fuzzy_int_max_len, suppress_warnings;
    const char * prepend;     // no longer in use
    SEXP env;                 // what envto look for functions to match call in
    int in_attr;
    int width;      // Tell alike what screen width to assume
    int env_limit;  // how many envs to track when searching for env loop
  };


  // - Main Funs --------------------------------------------------------------

  SEXP ALIKEC_alike (
    SEXP target, SEXP current, SEXP curr_sub, SEXP type_mode, SEXP attr_mode,
    SEXP env, SEXP fuzzy_int_max_len, SEXP suppress_warnings, SEXP lang_mode,
    SEXP width, SEXP env_limit
  );
  SEXP ALIKEC_alike_ext(SEXP target, SEXP current, SEXP cur_sub, SEXP env);
  SEXP ALIKEC_alike_ext2(SEXP target, SEXP current, SEXP curr_sub, SEXP env);

  SEXP ALIKEC_alike_fast1(
    SEXP target, SEXP current, SEXP curr_sub, SEXP settings
  );
  struct ALIKEC_res ALIKEC_alike_internal(
    SEXP target, SEXP current, struct ALIKEC_settings set
  );
  SEXP ALIKEC_typeof(SEXP object);
  SEXP ALIKEC_type_alike(SEXP target, SEXP current, SEXP mode, SEXP max_len);

  // - Internal Funs ----------------------------------------------------------

  SEXPTYPE ALIKEC_typeof_internal(SEXP object);
  struct ALIKEC_res_strings ALIKEC_type_alike_internal(
    SEXP target, SEXP current, int mode, R_xlen_t max_len
  );
  SEXP ALIKEC_compare_attributes(SEXP target, SEXP current, SEXP attr_mode);
  SEXP ALIKEC_compare_special_char_attrs(SEXP target, SEXP current);
  SEXP ALIKEC_res_msg_def(
    const char * tar_pre, const char * target,
    const char * act_pre, const char * actual
  );
  struct ALIKEC_res_sub ALIKEC_compare_attributes_internal(
    SEXP target, SEXP current, struct ALIKEC_settings set
  );
  SEXP ALIKEC_compare_class_ext(SEXP prim, SEXP sec);
  SEXP ALIKEC_compare_dimnames_ext(SEXP prim, SEXP sec);
  SEXP ALIKEC_compare_dim_ext(SEXP prim, SEXP sec, SEXP target, SEXP current);
  struct ALIKEC_res_sub ALIKEC_lang_alike_internal(
    SEXP target, SEXP current, struct ALIKEC_settings set
  );
  SEXP ALIKEC_lang_alike_ext(SEXP target, SEXP current, SEXP match_env);
  SEXP ALIKEC_lang_alike_chr_ext(SEXP target, SEXP current, SEXP match_env);
  struct ALIKEC_res_lang ALIKEC_lang_alike_rec(
    SEXP target, SEXP cur_par, pfHashTable * tar_hash, pfHashTable * cur_hash,
    pfHashTable * rev_hash, size_t * tar_varnum, size_t * cur_varnum,
    int formula, SEXP match_call, SEXP match_env, struct ALIKEC_settings set,
    struct ALIKEC_rec_track rec
  );
  struct ALIKEC_res_strings
    ALIKEC_fun_alike_internal(SEXP target, SEXP current);
  SEXP ALIKEC_fun_alike_ext(SEXP target, SEXP current);
  SEXP ALIKEC_compare_ts_ext(SEXP target, SEXP current);
  SEXP ALIKEC_pad_or_quote_ext(SEXP lang, SEXP width, SEXP syntactic);
  SEXP ALIKEC_res_strings_to_SEXP(struct ALIKEC_res_strings strings);

  // - Utility Funs -----------------------------------------------------------

  void psh(const char * lab);
  SEXP ALIKEC_rec_ind_as_lang(struct ALIKEC_rec_track rec);
  struct ALIKEC_rec_track ALIKEC_rec_def();
  struct ALIKEC_settings ALIKEC_set_def();
  struct ALIKEC_rec_track ALIKEC_rec_ind_chr(
    struct ALIKEC_rec_track res, const char * ind
  );
  struct ALIKEC_rec_track ALIKEC_rec_ind_num(
    struct ALIKEC_rec_track res, R_xlen_t ind
  );
  struct ALIKEC_res_sub ALIKEC_res_sub_def();
  SEXP ALIKEC_mode(SEXP obj);
  SEXP ALIKEC_test(SEXP obj);
  SEXP ALIKEC_test2(
    SEXP target, SEXP current
  );
  SEXP ALIKEC_getopt(const char * opt);
  SEXP ALIKEC_deparse_ext(SEXP obj, SEXP width_cutoff);
  SEXP ALIKEC_deparse_oneline_ext(
    SEXP obj, SEXP max_chars, SEXP keep_at_end
  );
  int ALIKEC_is_an_op(SEXP lang);
  int ALIKEC_is_an_op_inner(SEXP lang);
  const char * ALIKEC_pad_or_quote(SEXP lang, int width, int syntactic);
  SEXP ALIKEC_deparse_width(SEXP obj, int width);
  SEXP ALIKEC_deparse(SEXP obj, int width_cutoff);
  const char * ALIKEC_pad(SEXP obj, R_xlen_t lines, int pad);
  SEXP ALIKEC_pad_ext(SEXP obj, SEXP lines, SEXP pad);
  const char * ALIKEC_deparse_chr(SEXP obj, int width_cutoff);
  SEXP ALIKEC_match_call(SEXP call, SEXP match_call, SEXP env);
  SEXP ALIKEC_findFun(SEXP symbol, SEXP rho);
  SEXP ALIKEC_findFun_ext(SEXP symbol, SEXP rho);
  SEXP ALIKEC_string_or_true(struct ALIKEC_res_fin);
  SEXP ALIKEC_class(SEXP obj, SEXP class);
  SEXP ALIKEC_abstract_ts(SEXP x, SEXP what);
  int ALIKEC_env_track(SEXP env, struct ALIKEC_env_track * envs, int env_limit);
  SEXP ALIKEC_env_track_test(SEXP env, SEXP stack_size_init, SEXP env_limit);
  struct ALIKEC_env_track * ALIKEC_env_set_create(
    int stack_size_init, int env_limit
  );
  int ALIKEC_is_valid_name(const char *name);
  SEXP ALIKEC_is_valid_name_ext(SEXP name);
  int ALIKEC_is_dfish(SEXP obj);
  SEXP ALIKEC_is_dfish_ext(SEXP obj);
  struct ALIKEC_rec_track ALIKEC_rec_inc(struct ALIKEC_rec_track);
  struct ALIKEC_rec_track ALIKEC_rec_dec(struct ALIKEC_rec_track);
  SEXP ALIKEC_syntactic_names_exp(SEXP lang);
  SEXP ALIKEC_sort_msg(SEXP msgs);
  SEXP ALIKEC_merge_msg(SEXP msgs);
  SEXP ALIKEC_merge_msg_ext(SEXP msgs);

  // - Imported Funs ----------------------------------------------------------

  char * (*CSR_smprintf4)(
    size_t, const char *, const char *, const char *, const char *, const char *
  );
  char * (*CSR_smprintf6)(
    size_t, const char *, const char *, const char *, const char *, const char *,
    const char *, const char *
  );
  char * (*CSR_len_as_chr)(R_xlen_t);
  size_t (*CSR_strmlen)(const char *, size_t);
  size_t (*CSR_len_chr_len)(R_xlen_t);
  char * (*CSR_strmcpy)(const char * str, size_t maxlen);

  // - Init and pre-install Symbols -------------------------------------------

  SEXP ALIKEC_SYM_inherits;
  SEXP ALIKEC_SYM_package;
  SEXP ALIKEC_SYM_tilde;
  SEXP ALIKEC_SYM_paren_open;
  SEXP ALIKEC_SYM_args;
  SEXP ALIKEC_SYM_deparse;
  SEXP ALIKEC_SYM_nlines;
  SEXP ALIKEC_SYM_getOption;
  SEXP ALIKEC_SYM_matchcall;
  SEXP ALIKEC_SYM_widthcutoff;
  SEXP ALIKEC_CALL_matchcall;
  SEXP ALIKEC_CALL_matchcall_sub;
  SEXP ALIKEC_SYM_current;
  SEXP ALIKEC_SYM_attr;
  SEXP ALIKEC_SYM_attributes;
  SEXP ALIKEC_SYM_colnames;
  SEXP ALIKEC_SYM_length;
  SEXP ALIKEC_SYM_syntacticnames;
#endif
