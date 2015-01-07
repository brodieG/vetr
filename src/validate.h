#include <R.h>
#include <Rinternals.h>

// - Objects We Install Once ---------------------------------------------------

// One question: are static objects not garbage collected?  The examples from
// "Writing R Extensions" don't seem to require the protection of these

SEXP VALC_SYM_one_dot;
SEXP VALC_SYM_deparse;
SEXP VALC_SYM_paren;
SEXP VALC_SYM_quote;
SEXP VALC_TRUE;
SEXP(*VALC_alike)(SEXP,SEXP);
SEXP(*VALC_match_call)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
SEXP(*VALC_get_frame_data)(SEXP,SEXP,SEXP,int);
SEXP(*VALC_get_fun)(SEXP,SEXP);

SEXP VALC_validate();
SEXP VALC_test(SEXP a, SEXP b);
SEXP VALC_test1(SEXP a);
SEXP VALC_test2(SEXP a, SEXP b);
void VALC_stop(SEXP call, const char * msg);
void VALC_stop2(SEXP call, const char * msg, SEXP rho);
SEXP VALC_parse(SEXP lang, SEXP var_name, SEXP rho);
void VALC_parse_recurse(SEXP lang, SEXP lang_track, SEXP var_name, SEXP rho, int eval_as_is);
void VALC_install_objs();
SEXP VALC_evaluate(SEXP lang, SEXP arg_name, SEXP arg_value, SEXP lang_full, SEXP rho);
void VALC_arg_error(SEXP tag, SEXP fun_call, const char * err_base);
