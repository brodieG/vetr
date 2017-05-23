#include "cstringr.h"

/*
External interface to internal string functions, mostly for testing, note these
aren't very careful about potential overflows from input so user should ensure
inputs don't overflow int
*/

void is_scalar_pos_int(SEXP obj) {
  if(TYPEOF(obj) != INTSXP || XLENGTH(obj) != 1L || asInteger(obj) < 0)
    error("Argument `maxlen` must be a positive scalar integer");
}
void is_scalar_chr(SEXP obj) {
  if(TYPEOF(obj) != STRSXP || XLENGTH(obj) != 1L)
    error("Argument `str` must be a scalar character");
}
SEXP CSR_len_chr_len_ext(SEXP a) {
  is_scalar_pos_int(a);
  return ScalarInteger(CSR_len_chr_len((R_xlen_t) asInteger(a)));
}
SEXP CSR_len_as_chr_ext(SEXP a) {
  is_scalar_pos_int(a);
  return mkString(CSR_len_as_chr((R_xlen_t) asInteger(a)));
}
SEXP CSR_strmlen_ext(SEXP str, SEXP maxlen) {
  is_scalar_chr(str);
  is_scalar_pos_int(maxlen);
  return(ScalarInteger(CSR_strmlen(CHAR(asChar(str)), asInteger(maxlen))));
}
SEXP CSR_strmcpy_ext(SEXP str, SEXP maxlen) {
  is_scalar_chr(str);
  is_scalar_pos_int(maxlen);
  return(mkString(CSR_strmcpy(CHAR(asChar(str)), asInteger(maxlen))));
}
SEXP CSR_smprintf2_ext(SEXP maxlen, SEXP format, SEXP a, SEXP b) {
  is_scalar_chr(format);
  is_scalar_chr(a);
  is_scalar_chr(b);
  is_scalar_pos_int(maxlen);
  char * res = CSR_smprintf2(
    asInteger(maxlen), CHAR(asChar(format)), CHAR(asChar(a)), CHAR(asChar(b))
  );
  return mkString(res);
}
SEXP CSR_ucfirst_ext(SEXP str, SEXP maxlen) {
  is_scalar_chr(str);
  is_scalar_pos_int(maxlen);
  return(mkString(CSR_ucfirst(CHAR(asChar(str)), asInteger(maxlen))));
}
SEXP CSR_lcfirst_ext(SEXP str, SEXP maxlen) {
  is_scalar_chr(str);
  is_scalar_pos_int(maxlen);
  return(mkString(CSR_lcfirst(CHAR(asChar(str)), asInteger(maxlen))));
}
SEXP CSR_bullet_ext(SEXP str, SEXP bullet, SEXP ctd, SEXP maxlen) {
  if(TYPEOF(str) != STRSXP || TYPEOF(bullet) != STRSXP || TYPEOF(ctd) != STRSXP)
    error("First three arguments must be string");
  if(TYPEOF(maxlen) != INTSXP) error("Argument `maxlen` must be integer");
  if(XLENGTH(bullet) != 1) error("Argument `bullet` must be length 1");
  if(XLENGTH(ctd) != 1) error("Argument `ctd` must be length 1");

  R_xlen_t i, str_len = XLENGTH(str);
  SEXP res = PROTECT(allocVector(STRSXP, str_len));

  const char * chr_bul = CHAR(STRING_ELT(bullet, 0));
  const char * chr_ctd = CHAR(STRING_ELT(ctd, 0));
  size_t st_ml = INTEGER(maxlen)[0];

  if(str_len) {
    for(i = 0; i < str_len; ++i) {
      const char * char_orig = CHAR(STRING_ELT(str, i));
      const char * char_new = CSR_bullet(char_orig, chr_bul, chr_ctd, st_ml);

      SET_STRING_ELT(res, i, mkChar(char_new));
  } }
  UNPROTECT(1);
  return res;
}

