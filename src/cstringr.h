/*
Copyright (C) 2023 Brodie Gaslam

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
#include <stdint.h>
#include <ctype.h>

#ifndef _CSTRINGR_H
#define _CSTRINGR_H

  // Constants

#define CSR_MAX_CHAR 50000

  // Testing Functions

  SEXP CSR_len_chr_len_ext(SEXP a);
  SEXP CSR_len_as_chr_ext(SEXP a);
  SEXP CSR_strmlen_ext(SEXP str, SEXP maxlen);
  SEXP CSR_strmcpy_ext(SEXP str, SEXP maxlen);
  SEXP CSR_smprintf2_ext(SEXP maxlen, SEXP format, SEXP a, SEXP b);
  SEXP CSR_smprintf6_ext(
    SEXP maxlen, SEXP format, SEXP a, SEXP b, SEXP c, SEXP d, SEXP e, SEXP f
  );
  SEXP CSR_ucfirst_ext(SEXP str, SEXP maxlen);
  SEXP CSR_lcfirst_ext(SEXP str, SEXP maxlen);
  SEXP CSR_bullet_ext(SEXP str, SEXP bullet, SEXP ctd, SEXP maxlen);
  SEXP CSR_collapse_ext(SEXP str, SEXP sep, SEXP maxlen);

  SEXP CSR_strsub(SEXP string, SEXP chars, SEXP mark_trunc);
  SEXP CSR_nchar_u(SEXP string);
  SEXP CSR_char_offsets(SEXP string);

  SEXP CSR_test_strmcpy(void);
  SEXP CSR_test_strappend(void);
  SEXP CSR_test_strappend2(void);
  SEXP CSR_test_add_szt(void);
  SEXP CSR_test_smprintfx(void);

  // Internal Functions

  size_t CSR_len_chr_len(R_xlen_t a);
  char * CSR_len_as_chr(R_xlen_t a);
  char * CSR_num_as_chr(double a, int as_int);
  SEXP CSR_num_as_chr_ext(SEXP a, SEXP as_int);
  size_t CSR_strmlen_x(const char * str, size_t maxlen);
  size_t CSR_strmlen(const char * str, size_t maxlen);
  char * CSR_strmcpy(const char * str, size_t maxlen);
  char * CSR_strmcpy_int(const char * str, size_t maxlen, int warn);
  char * CSR_smprintf6(
    size_t maxlen, const char * format, const char * a, const char * b,
    const char * c, const char * d, const char * e, const char * f
  );
  char * CSR_smprintf5(
    size_t maxlen, const char * format, const char * a, const char * b,
    const char * c, const char * d, const char * e
  );
  char * CSR_smprintf4(
    size_t maxlen, const char * format, const char * a, const char * b,
    const char * c, const char * d
  );
  char * CSR_smprintf3(
    size_t maxlen, const char * format, const char * a, const char * b,
    const char * c
  );
  char * CSR_smprintf2(
    size_t maxlen, const char * format, const char * a, const char * b
  );
  char * CSR_smprintf1(size_t maxlen, const char * format, const char * a);
  const char * CSR_bullet(SEXP string, SEXP bullet, SEXP ctd, size_t max_len);

  char * CSR_ucfirst(const char * str, size_t maxlen);
  char * CSR_lcfirst(const char * str, size_t maxlen);

  char * CSR_collapse(SEXP str, const char *, size_t maxlen);

  void CSR_strappend(char * target, const char * str, size_t maxlen);

  size_t CSR_add_szt(size_t a, size_t b);

  // macros, offset is expected to be a pointer to a character

  #define UTF8_IS_CONT(offset) UTF8_BW(offset, 0x80, 0xBF)
  #define UTF8_BW(offset, a, b) ((*(offset) >= (a)) && (*(offset) <= (b)))

#endif
