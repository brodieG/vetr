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

#include "cstringr.h"
/*
 * Most of these functions were written as a learning exercise in C string
 * manipulation.  In most cases they are not strictly necessary.  Additionally,
 * most of them try to be "safe" by not reading past "maxlen", but in many of
 * our uses we don't actually know the buffer size ahead of time and use a
 * generic maxlen value, which pretty much defeats the purpose...
 *
 * There isn't really any real harm assuming the strings are terminated, which
 * they should be since they are either retrieved from CHRSXPs, or generated
 * internally.
 *
 * DO NOT USE THESE STRING MANIPULATION FUNS OUTSIDE OF THIS PACKAGE.
 */

/* Estimate how many characters a R_xlen_t number can be represented with
 *
 * Implicitly we're assuming R_XLEN_T_MAX < DOUBLE_MAX, which seems like a
 * pretty safe assumption (see assumptions.c).
 */

size_t CSR_len_chr_len(R_xlen_t a) {
  if(a < 0) {
    // nocov start
    error("Logic Error: unexpected negative length value; contact maintainer");
    // nocov end
  }
  // + 1.00001 to account for 0
  size_t log_len = (size_t) ceil(log10((double) a + 1.00001));
  return log_len;
}
/*
Returns a character pointer to the string representation of the integer;
allocates with R_alloc so in theory don't need to worry about freeing memory
*/

char * CSR_len_as_chr(R_xlen_t a) {
  return CSR_num_as_chr((double)a, 1);
}
/*
 * Convert number to character representation
 *
 * Using .0f due to portability issues.  used to be %td, but doesn't work on
 * windows, then %zd apparently doesn't work on the mingw compiler (at least
 * without tweaks), so we're trying doubles which in theory should represent
 * anything we could possibly get from R_xlen_t.
 *
 * Note this will try to print as integer if a number can be precisely
 * represented as an integer.  No accounting for precision is made, so there
 * must be nothing past decimal point for this to happen.
 */

char * CSR_num_as_chr(double num, int as_int) {
  char * res = "<INTERNAL ERROR>"; // should always be modified

  // Handle special cases

  if(ISNA(num)) {
    res = "NA";
  } else if (ISNAN(num)) {
    res = "NaN";
  } else if (num == R_PosInf) {
    res = "Inf";
  } else if (num == R_NegInf) {
    res = "-Inf";
  } else {
    const char * format;
    double max_num_int = pow((double) 2, 53);
    if(as_int &&  (max_num_int < num || -max_num_int > num)) {
      // nocov start
      error(
        "%s%s",
        "Internal Error: can't handle values greater than 2^53 in int mode, ",
        "contact maintainer."
      );
      // nocov end
    }
    // Force int display for exact ints

    if(
      !as_int && max_num_int >= num && -max_num_int <= num && !fmod(num, 1)
    )
      as_int = 1;

    // Otherwise use floating or scientific if abs value greater than a billion,
    // not exactly the same as what R does

    if(as_int) format = "%.0f";
    else if (fabs(num) >= 1e9) format = "%e";
    else format = "%f";

    // not clear what snprintf does if mem_req would be greater than INT_MAX,
    // doesn't appear documented.  Likely impossible to hit that here though

    int mem_req = snprintf(NULL, 0, format, num);
    if(mem_req < 0)
      // nocov start
      error(
        "%s%s",
        "Internal Error: could not compute as character width of number, ",
        "contact maintainer."
      );
      // nocov end
    res = R_alloc(mem_req + 1, sizeof(char));
    int write_res = snprintf(res, mem_req + 1, format, num);

    if(write_res < 0)
      // nocov start
      error(
        "%s%s", "Internal Error: failed converting num to string, ",
        "contact maintainer."
      );
      // nocov end
    else if(write_res > mem_req)
      // nocov start
      error(
        "%s%s", "Internal Error: truncation converting num to string, ",
        "contact maintainer."
      );
      // nocov end
  }
  return res;
}
SEXP CSR_num_as_chr_ext(SEXP a, SEXP as_int) {
  return mkString(CSR_num_as_chr(asReal(a), asInteger(as_int)));
}
/*
A slightly safer strmlen, errors if exceeds `maxlen`

Not really safe since there is no actual guarantee that the allocated buffer is longer than maxlen or that it contains character 0.

If success, returns size of string excluding the NULL terminator.  If the NULL
terminator is not found prior to `maxlen`, then errors.

See CSR_strmlen_x for a version of this function that does not error, but
careful using it with strcpy as you could get an overflow.  CSR_strmcpy will
stop copying upon hitting `maxlen` and will add a NULL terminator so it is safe
to use CSR_strmlen with that.

partly from insane blogger
*/
size_t CSR_strmlen(const char * str, size_t maxlen) {
  size_t res = CSR_strmlen_x(str, maxlen);
  if(res == maxlen && *(str + res)) {
    // reached max len and next charcter is not NULL terminator
    error("%s %s %d %s",
      "Internal Error (CSR_strmlen): failed to find string terminator prior",
      "to maxlen", maxlen, "characters"
    );
  }
  return res;
}
/*
IMPORTANT: only use with CSR_strmcpy since that will not try to copy past maxlen

Used to be implemented with `memchr` but we removed that because it seemed to
want to read past buffer ends.  Some docs suggest this is possible given that it
reads full words in (or maybe even multiple words), though that would also
require for R_alloc to allocate non-full words, which seems odd.

Length returned does not include the NULL terminator.
*/
size_t CSR_strmlen_x(const char * str, size_t maxlen) {

  const char * str_i = str;
  if(maxlen > UINTPTR_MAX)
    // nocov start
    error("Internal error in strmlen, maxlen > UINTPTR_MAX not support");
    // nocov end
  if((uintptr_t)str > UINTPTR_MAX - maxlen)
    // nocov start
    error(
      "Internal error in strmlen, maxlen would imply pointer overflow"
    );
    // nocov end

  const char * str_end = str + maxlen;

  while(*str_i && str_i < str_end) ++str_i;

  return (size_t) (str_i - str);
}
/*
If str has more than size characters, returns a copy of str truncated to size
characters with a null character appended such that strmlen() str == size,
otherwise returns a copy of str.

This operates on bytes, so will potentially break UTF-8 and other multi-byte
encoded strings.

Note, final string size could be up to maxlen + 1 including the NULL terminator.
A NULL terminator is always added at the end of the string.
*/
char * CSR_strmcpy_int(const char * str, size_t maxlen, int warn) {
  if(!maxlen) return("");
  if(maxlen == SIZE_MAX)
    error("Argument `maxlen` must be at least one smaller than SIZE_MAX.");

  size_t len = CSR_strmlen_x(str, maxlen);
  if(warn && len == maxlen && str[len])
    warning("CSR_strmcpy: truncated string longer than %d", maxlen);

  char * str_new = R_alloc(len + 1, sizeof(char));

  // should we use memcpy?
  if(len) {
    if(!strncpy(str_new, str, len)) {
      // nocov start
      error("%s%s",
        "Internal Error (CSR_strncopy): failed making copy of string for  ",
        "truncation; contact maintainer."
      );
      // nocov end
    }
  }
  // Ensure null terminated.

  str_new[len] = '\0';

  return str_new;
}
char * CSR_strmcpy(const char * str, size_t maxlen) {
  return CSR_strmcpy_int(str, maxlen, 1);
}
/*
 * Like CSR_strmcpy, but copies to a presupplied pointer (`target`).  In this
 * way it is much closer to strncpy, except that it truncates everything at
 * `maxlen`.
 *
 * Note this requires that you pre-compute and allocate `str` properly.
 *
 * Probably would be a bit more efficient if we sent over the lenght of the
 * object instead of having to measure it here, but this is fast enough as is.
 */
void CSR_strappend(char * target, const char * str, size_t maxlen) {
  if(maxlen) {
    if(maxlen > SIZE_MAX - 1) {
      error("%s%s",
        "Argument `maxlen` must be at least one smaller than max possible ",
        "size_t value."
      );
    }
    size_t len = CSR_strmlen_x(str, maxlen);

    if(len == maxlen && str[len])
      warning("CSR_strmcopy: truncated string longer than %d", maxlen);

    if(len) {
      if(!strncpy(target, str, len)) {
        // nocov start
        error("%s%s",
          "Internal Error (CSR_strappend): failed making copy of string for  ",
          "truncation; contact maintainer."
        );
        // nocov end
      }
    }
    // Ensure null terminated if last character is not NULL; this happens when
    // truncating to `maxlen`, also if zero len make sure that is a NULL

    target[len] = '\0';
  }
}
/*
 * Add two size_t if possible, error otherwise
 */
size_t CSR_add_szt(size_t a, size_t b) {
  if(SIZE_MAX - a < b)
    error("%s%s",
      "size_t overflow: you tried to add two size_t numbers that together ",
      "overflow size_t"
    );
  return a + b;
}
/*
Returns a character pointer containing the results of using `a` as the parent
string and all the others a substrings with `sprintf`

note:
- will over-allocate by the amount of formatting characters
- maxlen limits the length of individual components and the formatting string,
  not the output; because you can only specify one maxlen and there are multiple
  inputs in addition to the formatting string maxlen can't be used to prevent
  reading past buffer size if any of them are smaller than maxlen.  Basically,
  the underlying assumption with all of these is that all inputs are NULL
  terminated
- If you submit more %s tokens than there are args, bad stuff starts to
  happen and we don't actually check the formatting tokens
*/

char * CSR_smprintf6(
  size_t maxlen, const char * format, const char * a, const char * b,
  const char * c, const char * d, const char * e, const char * f
) {
  size_t full_len;
  size_t format_len = CSR_strmlen_x(format, maxlen);
  if(format_len >= maxlen)
    error(
      "Internal Error: formatting string length longer that `nchar.max` %s,",
      "contact maintainer."
    );
  full_len = CSR_add_szt(format_len, CSR_strmlen_x(a, maxlen));
  full_len = CSR_add_szt(full_len, CSR_strmlen_x(b, maxlen));
  full_len = CSR_add_szt(full_len, CSR_strmlen_x(c, maxlen));
  full_len = CSR_add_szt(full_len, CSR_strmlen_x(d, maxlen));
  full_len = CSR_add_szt(full_len, CSR_strmlen_x(e, maxlen));
  full_len = CSR_add_szt(full_len, CSR_strmlen_x(f, maxlen));

  char * res;

  // Limit each string and format to `maxlen`

  char * a_cpy = CSR_strmcpy(a, maxlen);
  char * b_cpy = CSR_strmcpy(b, maxlen);
  char * c_cpy = CSR_strmcpy(c, maxlen);
  char * d_cpy = CSR_strmcpy(d, maxlen);
  char * e_cpy = CSR_strmcpy(e, maxlen);
  char * f_cpy = CSR_strmcpy(f, maxlen);

  res = R_alloc(full_len + 1, sizeof(char));
  int res_len = sprintf(
    res, CSR_strmcpy(format, maxlen), a_cpy, b_cpy, c_cpy, d_cpy, e_cpy, f_cpy
  );
  if(res_len < 0) {
    // nocov start
    error("%s%s",
      "Internal Error (CSR_smprintf): `sprintf` returned -1 when generating ",
      "new string"
    );
    // nocov end
  }
  return res;
}
char * CSR_smprintf5(
  size_t maxlen, const char * format, const char * a, const char * b,
  const char * c, const char * d, const char * e
) {
  return(CSR_smprintf6(maxlen, format, a, b, c, d, e, ""));
}
char * CSR_smprintf4(
  size_t maxlen, const char * format, const char * a, const char * b,
  const char * c, const char * d
) {
  return(CSR_smprintf6(maxlen, format, a, b, c, d, "", ""));
}
char * CSR_smprintf3(
  size_t maxlen, const char * format, const char * a, const char * b,
  const char * c
) {
  return(CSR_smprintf6(maxlen, format, a, b, c, "", "", ""));
}
char * CSR_smprintf2(
  size_t maxlen, const char * format, const char * a, const char * b
) {
  return(CSR_smprintf6(maxlen, format, a, b, "", "", "", ""));
}
char * CSR_smprintf1(size_t maxlen, const char * format, const char * a) {
  return(CSR_smprintf6(maxlen, format, a, "", "", "", "", ""));
}
// - Capitalization functions --------------------------------------------------

/* Make copy and capitalize first letter */

char * CSR_ucfirst(const char * str, size_t maxlen) {
  char * str_new = (char *) CSR_strmcpy(str, maxlen);
  str_new[0] = toupper(str_new[0]);
  return str_new;
}
char * CSR_lcfirst(const char * str, size_t maxlen) {
  char * str_new = (char *) CSR_strmcpy(str, maxlen);
  str_new[0] = tolower(str_new[0]);
  return str_new;
}
/*
 * Makes a copy of string, and adds `bullet` at the beginning, and `ctd` after
 * each newline
 *
 * Remember we could end up allocating 1 more than max_len
 */

const char * CSR_bullet(
  SEXP string, SEXP bullet, SEXP ctd, size_t max_len
) {
  if(
    TYPEOF(string) != CHARSXP || TYPEOF(bullet) != CHARSXP ||
    TYPEOF(ctd) != CHARSXP
  )
    error("Internal Error: requires charsxp, contact maintainer."); // nocov

  size_t newlines=0;
  size_t chars=0;
  const char * string_copy = CHAR(string);

  while(*string_copy) {
    if(*string_copy == '\n' && *(string_copy + 1)) ++newlines;
    ++string_copy;
    ++chars;

    if(chars > max_len)
      error("Exceeded `max_len` when trying to bullet `string`");
  }
  size_t ctd_size = (size_t) LENGTH(ctd);
  size_t bullet_size = (size_t) LENGTH(bullet);

  // Add all numbers together in a way that checks for overflows

  size_t size_all = CSR_add_szt(string_copy - CHAR(string), 1);
  size_all = CSR_add_szt(size_all, bullet_size);
  for(size_t i = 0; i < newlines; ++i)
    size_all = CSR_add_szt(size_all, ctd_size);

  if(size_all > max_len)
    error("Exceeded `max_len` when trying to bullet `string` (2)");

  // Now allocate

  char * res = R_alloc(size_all, sizeof(char));
  char * res_cpy  = res;

  // Second pass, copy stuff to our result string, start by adding the bullet

  strcpy(res_cpy, CHAR(bullet));
  res_cpy += bullet_size;

  string_copy = CHAR(string);
  while(*string_copy) {
    int add_ctd = 0;
    *res_cpy = *string_copy;
    if(*res_cpy == '\n') add_ctd = 1;
    ++res_cpy;
    ++string_copy;
    if(add_ctd && *(string_copy)) {
      strcpy(res_cpy, CHAR(ctd));
      res_cpy += ctd_size;
    }
    // *(res_cpy + 1) = '\0';  // so we can Rprintf
  }
  *(res_cpy) = '\0';

  return res;
}
/*
 * Collapse STRSXP into one chr *
 *
 * Strings will get truncate dat `max_len` if they are longer than that
 */

char * CSR_collapse(SEXP str, const char * sep, size_t max_len) {
  if(TYPEOF(str) != STRSXP) error("Argument `str` must be a character vector");

  R_xlen_t str_len = XLENGTH(str);
  if(!str_len) {
    return "";
  } else {
    // Compute sizes

    size_t size_all = 0;
    size_t sep_len = CSR_strmlen_x(sep, max_len);
    R_xlen_t i;

    for(i = 0; i < str_len; i++) {
      size_all =
        CSR_add_szt(size_all, CSR_strmlen_x(CHAR(STRING_ELT(str, i)), max_len));
      if(i < str_len - 1) {
        size_all = CSR_add_szt(size_all, sep_len);
      }
    }
    max_len = size_all;
    // Allocate and generate string

    char * str_new = R_alloc(max_len + 1, sizeof(char));
    char * str_cpy = str_new;

    for(i = 0; i < str_len; i++) {
      const char * to_copy = CHAR(STRING_ELT(str, i));
      CSR_strappend(str_cpy, to_copy, max_len);
      size_t copy_len = CSR_strmlen_x(to_copy, max_len);
      str_cpy += copy_len;
      // nocov start
      if(max_len < copy_len) error("Internal error: exhaused copy buffer.");
      // nocov end
      max_len -= copy_len;
      if(i < str_len - 1) {
        CSR_strappend(str_cpy, sep, max_len);
        str_cpy += sep_len;
        // nocov start
        if(max_len < sep_len) error("Internal error: exhaused copy buffer 2.");
        // nocov end
        max_len -= sep_len;
      }
    }
    *str_cpy = '\0';
    return str_new;
  }
}
SEXP CSR_collapse_ext(SEXP str, SEXP sep, SEXP max_len) {
  return mkString(CSR_collapse(str, CHAR(asChar(sep)), INTEGER(max_len)[0]));
}
