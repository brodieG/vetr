#include <R.h>
#include <Rinternals.h>
#include <ctype.h>

// - Objects We Install Once ---------------------------------------------------

// One question: are static objects not garbage collected?  The examples from
// "Writing R Extensions" don't seem to require the protection of these

SEXP VALC_SYM_one_dot;
SEXP VALC_SYM_deparse;
SEXP VALC_SYM_paren;
SEXP VALC_SYM_quote;
SEXP VALC_SYM_current;
SEXP VALC_TRUE;
SEXP VALC_SYM_errmsg;

SEXP VALC_validate(
  SEXP target, SEXP current, SEXP cur_sub, SEXP par_call, SEXP rho,
  SEXP ret_mode_sxp, SEXP stop
);
SEXP VALC_validate_args();
SEXP VALC_remove_parens(SEXP lang);
SEXP VALC_name_sub_ext(SEXP symb, SEXP arg_name);
SEXP VALC_test(SEXP a, SEXP b);
SEXP VALC_test1(SEXP a);
SEXP VALC_test2(SEXP a, SEXP b);
void VALC_stop(SEXP call, const char * msg);
void VALC_stop2(SEXP call, const char * msg, SEXP rho);
SEXP VALC_all_ext(SEXP vec);
int VALC_all(SEXP vec);
int IS_TRUE(SEXP x);
int IS_LANG(SEXP x);
SEXP VALC_parse(SEXP lang, SEXP var_name, SEXP rho);
void VALC_parse_recurse(SEXP lang, SEXP lang_track, SEXP var_name, SEXP rho, int eval_as_is, SEXP first_fun);
SEXP VALC_sub_symbol(SEXP lang, SEXP rho);
void VALC_install_objs();
SEXP VALC_evaluate(SEXP lang, SEXP arg_lang, SEXP arg_tag, SEXP arg_value, SEXP lang_full, SEXP rho);
void VALC_arg_error(SEXP tag, SEXP fun_call, const char * err_base);

// - Imported Funs -------------------------------------------------------------

SEXP(*VALC_alike)(SEXP,SEXP,SEXP,SEXP);
SEXP(*VALC_match_call)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
SEXP(*VALC_get_frame_data)(SEXP,SEXP,SEXP,int);
SEXP(*VALC_get_fun)(SEXP,SEXP);
SEXP(*VALC_deparse)(SEXP,int);
const char * (*VALC_pad)(SEXP,int,int);
const char * (*VALC_bullet)(const char *, const char *, const char *, size_t);
const char * (*VALC_pad_or_quote)(SEXP,int,int);

// String Maniip

char * (*CSR_smprintf4)(
  size_t, const char *, const char *, const char *, const char *, const char *
);
char * (*CSR_strmcpy)(const char *, size_t);
size_t (*CSR_strmlen)(const char *, size_t);

#ifndef VALC_MAX_CHAR
# define VALC_MAX_CHAR 10000
#endif

