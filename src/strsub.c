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
 * This appears to update with `Sys.setlocale`, but super annoyingly we get a
 * CMD check note about this, so we need to resort to parsing `Sys.getlocale`
 * which seems more than suboptimal.  Oddly, mbcslocale doesn't trigger the same
 * note.
 */
// extern Rboolean utf8locale;

/*
 * Most of the functionality in this file already exists built in to R, so we
 * suggest you check out `nchar`, `substr`, etc.  This is mostly a learning
 * exercise for me.
 *
 */
static int is_utf8_enc(cetype_t type) {
  SEXP l10n_info = PROTECT(install("l10n_info"));
  SEXP l10n_call = PROTECT(lang1(l10n_info));

  int err_val = 0;
  SEXP l10n = PROTECT(R_tryEval(l10n_call, R_BaseEnv, &err_val));
  if(err_val)
    // nocov start
    error("Internal Error: failed getting UTF8 locale; contact maintainer.");
    // nocov end

  if(TYPEOF(l10n) != VECSXP)
    error("Internal Error: l10n_info did not return a list."); // nocov

  SEXP l10n_names = getAttrib(l10n, R_NamesSymbol);
  if(TYPEOF(l10n_names) != STRSXP)
    error("Internal Error: l10n_info did not return a named list."); // nocov

  int utf8_loc = 0;
  for (R_xlen_t i = 0; i < XLENGTH(l10n_names); ++i) {
    if(!strcmp(CHAR(STRING_ELT(l10n_names, i)), "UTF-8")) {
      if(TYPEOF(VECTOR_ELT(l10n, i)) != LGLSXP)
        error("Internal Error: l10n_info()$`UTF-8` is not logical."); // nocov
      utf8_loc = asInteger(VECTOR_ELT(l10n, i));
      break;
  } }
  int res = (type == CE_UTF8) || (type == CE_NATIVE && utf8_loc);

  UNPROTECT(3);
  return res;
}
/*
 * Computes how many bytes a character take.
 *
 * For UTF-8, computes the length, for all others just returns 1.
 *
 * If possible UTF8 outside ASCII, check up to the next 4 bytes and for and
 * offset bytes by length of maximal subpart of valid sequence, or length of
 * valid sequence.  Note that we've already advanced one byte above so all
 * failing cases don't need to advance further as they would advance by one.
 *
 * Note R implements `utf8clen` in R3.4.0/src/main/util.c@1193 in a far more
 * efficient but less precise manner in as much as it doesn't implement table
 * 3-7 exactly..
 *
 * This is based on:
 * <http://www.unicode.org/versions/Unicode10.0.0/ch03.pdf#G7404>,
 * table 3-7, transcribed below
 *
 * Well-Formed UTF-8 Byte Sequences
 * Code Points        | Byte 1 | Byte 2 | Byte 3 | Byte 4
 * U+0000..U+007F     | 00..7F |
 * U+0080..U+07FF     | C2..DF | 80..BF
 * U+0800..U+0FFF     | E0     |*A0..BF*| 80..BF
 * U+1000..U+CFFF     | E1..EC | 80..BF | 80..BF
 * U+D000..U+D7FF     | ED     |*80..9F*| 80..BF
 * U+E000..U+FFFF     | EE..EF | 80..BF | 80..BF
 * U+10000..U+3FFFF   | F0     |*90..BF*| 80..BF | 80..BF
 * U+40000..U+FFFFF   | F1..F3 | 80..BF | 80..BF | 80..BF
 * U+100000..U+10FFFF | F4     |*80..8F*| 80..BF | 80..BF
 *
 * @return integer length of UT8 sequence starting at char_ptr, as a negative
 *   value if the sequence is invalid (so values could be -3, -2, 1, 1, 2, 3, 4)
 */
static inline int char_offset(unsigned const char * char_ptr, int is_bytes) {
  unsigned const char char_val = *(char_ptr);

  int byte_count = 1;  // Always at least one val
  // failures should be rare, so start with success to save the operation on
  // success; code is more complicated as a result though...
  int success = 1;

  // Everything other than CE_BYTES should have been converted to UTF8 or be
  // UTF8/ASCII

  if(!is_bytes && char_val & 128) {
    if(UTF8_BW(char_ptr, 0xC2, 0xDF)) {
      // two byte sequence
      if(UTF8_IS_CONT(char_ptr + 1)) {
        byte_count +=1;
      } else success = 0;
    } else if(char_val == 0xE0) {
      // three byte sequence, exception 1
      if(UTF8_BW(char_ptr + 1, 0xA0, 0xBF)) {
        if(UTF8_IS_CONT(char_ptr + 2)) byte_count +=2;
        else {
          byte_count +=1;
          success = 0;
        }
      } else success = 0;
    } else if(char_val == 0xED) {
      // three byte sequence, exception 2
      if(UTF8_BW(char_ptr + 1, 0x80, 0x9F)) {
        if(UTF8_IS_CONT(char_ptr + 2)) byte_count +=2;
        else {
          byte_count +=1;
          success = 0;
        }
      } else success = 0;
    } else if (UTF8_BW(char_ptr, 0xE0, 0xEF)) {
      // three byte sequence normal, note by construction excluding E0, ED
      if(UTF8_IS_CONT(char_ptr + 1)) {
        if(UTF8_IS_CONT(char_ptr + 2)) byte_count +=2;
        else {
          byte_count +=1;
          success = 0;
        }
      } else success = 0;
    } else if (char_val == 0xF0) {
      // four byte sequence, v1
      if(UTF8_BW(char_ptr + 1, 0x90, 0xBF)) {
        if(UTF8_IS_CONT(char_ptr + 2)) {
          if(UTF8_IS_CONT(char_ptr + 3)) {
            byte_count += 3;
          } else {
            byte_count += 2;
            success = 0;
          }
        } else {
          byte_count +=1;
          success = 0;
        }
      } else success = 0;
    } else if (UTF8_BW(char_ptr, 0xF1, 0xF3)) {
      // four byte sequence, v2
      if(UTF8_IS_CONT(char_ptr + 1)) {
        if(UTF8_IS_CONT(char_ptr + 2)) {
          if(UTF8_IS_CONT(char_ptr + 3)) {
            byte_count += 3;
          } else {
            byte_count += 2;
            success = 0;
          }
        } else {
          byte_count +=1;
          success = 0;
        }
      } else success = 0;
    } else if (char_val == 0xF4) {
      // four byte sequence, v3
      if(UTF8_BW(char_ptr + 1, 0x80, 0x8F)) {
        if(UTF8_IS_CONT(char_ptr + 2)) {
          if(UTF8_IS_CONT(char_ptr + 3)) {
            byte_count += 3;
          } else {
            byte_count += 2;
            success = 0;
          }
        } else {
          byte_count +=1;
          success = 0;
        }
      } else success = 0;
    } else success = 0;
  }
  return success ? byte_count : -byte_count;
}
/*
 * Rather than try to handle all native encodings, we just convert directly
 * to UTF8 and don't worry about it.  This will be sub-optimal in LATIN1 or
 * windows 1252 locales where each character can be represented by a byte
 * and could potentially lead to copying of entire vectors. We'll have to
 * consider a mode where we let known 255 element encodings through...
 *
 * Note that CE_BYTES encoding is left as is.  Unfortunately this means that all
 * output from this function needs to check whether the original type was bytes
 * or not.
 *
 * @param string a CHARSXP
 */
static inline unsigned const char * as_utf8_char(SEXP string) {
  const char * char_val;

  cetype_t char_enc = getCharCE(string);
  if(is_utf8_enc(char_enc) || char_enc == CE_BYTES) {
    char_val = CHAR(string);
  } else {
    char_val = translateCharUTF8(string);
  }
  return (unsigned const char *) char_val;
}
/*
 * Truncates strings to specified length.
 *
 * Will re-encode any encoded strings into UTF-8.
 *
 * string character vector of strings to truncate
 * len scalar integer of length to truncate to
 * mark_trunc scalar logical whether to append a ".." to indicate that
 *   a string was truncated
 *
 * Note that we always allocate a STRSXP of the same length as `string`, even if
 * we end up not trimming any of the elements.  We only create new CHARSXP for
 * the elements that are changed (assuming it's okay to re-use the same CHARSXP
 * in different strings).
 */

SEXP CSR_strsub(SEXP string, SEXP chars, SEXP mark_trunc) {
  if(TYPEOF(string) != STRSXP)
    error("Argument `string` must be a string.");
  if(TYPEOF(mark_trunc) != LGLSXP && xlength(mark_trunc) != 1)
    error("Argument `mark_trunc` must be a TRUE or FALSE.");

  if(
    TYPEOF(chars) != INTSXP || xlength(chars) != 1 || INTEGER(chars)[0] < 1
  )
    error(
      "Argument `chars` must be scalar integer, strictly positive, and not NA."
    );

  R_xlen_t i, len = xlength(string);
  int mark = asInteger(mark_trunc) > 0;
  int chars_int = asInteger(chars);

  // If you change this, you must adapt position tracking below to make sure you
  // keep track of the right byte position for offset -pad_len

  const char * pad = ".."; // padding for truncated strings
  const int pad_len = 2;   // make sure this aligns with `pad`

  if(chars_int - mark * pad_len < 1)
    error(
      "Argument `chars` must be greater than 2 when `mark_trunc` is TRUE."
    );

  SEXP res_string = PROTECT(allocVector(STRSXP, len));

  for(i = 0; i < len; ++i) {
    unsigned const char * char_start, * char_ptr;
    unsigned char char_val; // need for > 127

    SEXP char_cont = STRING_ELT(string, i);
    cetype_t char_enc = getCharCE(char_cont);
    char_start = as_utf8_char(char_cont);

    R_xlen_t char_count = 0;

    // Limiting to 8 less than SIZE_T_MAX to make room for a last 4 byte UTF8
    // character, '..', and the NULL terminator

    int byte_count = 0, byte_count_prev = 0, byte_count_prev_prev = 0;
    int is_utf8 = 0;
    int byte_off = 0;

    // Loop while no NULL character

    while(
      (char_val = *(char_ptr = (char_start + byte_count))) &&
      char_count < chars_int
    ) {
      // Keep track of the byte position two characters ago

      if(char_count > 1) byte_count_prev_prev = byte_count_prev;
      if(char_count) byte_count_prev = byte_count;

      ++char_count;
      byte_off = abs(char_offset(char_ptr, char_enc == CE_BYTES));
      if(byte_count > INT_MAX - byte_off)
        // nocov start
        error(
          "Internal Error: string longer than INT_MAX bytes encountered."
        );
        // nocov end
      byte_count += byte_off;
      if(char_val > 127 && char_enc != CE_BYTES) is_utf8 = 1;
    }
    // Check whether we got to end of string, and if we did truncate

    SEXP char_sxp;

    if(char_count >= chars_int && char_val) {
      char * char_res;
      char * char_trunc = CSR_strmcpy_int(
        (const char *) char_start, mark ? byte_count_prev_prev : byte_count, 0
      );
      if(mark) {
        // add an ellipsis at the end.  This is inefficient since we copy the
        // string again, but probably not worth the work to do it in one step.
        // Also probably don't need the CSR fun.

        char_res = R_alloc(byte_count + 1, sizeof(char));
        int snp_try = snprintf(
          char_res, byte_count + 1, "%s%s", char_trunc, pad
        );
        if(snp_try < 0)
          // nocov start
          error(
            "Internal Error: failed generating truncated string at index %.0f",
            (double) i
          );
          // nocov end
      } else char_res = char_trunc;

      char_sxp = PROTECT(mkCharCE(char_res, is_utf8 ?  CE_UTF8 : char_enc));
    } else {
      char_sxp = PROTECT(STRING_ELT(string, i));
    }
    // Deal with incorrectly encoded strings? At this point we just let them
    // through since presumably `translateToUTF8` should have dealt with them

    SET_STRING_ELT(res_string, i, char_sxp);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return res_string;
}
/*
 * Like `nchar`, but has a homegrown implementation of UTF8 character counting.
 *
 * Likely to be relatively slow for non UTF8 strings since for those we could
 * just use LENGTH(<CHARSXP>) but we're not bothering with that for now.
 */
SEXP CSR_nchar_u(SEXP string) {
  if(TYPEOF(string) != STRSXP)
    error("Argument `string` must be a character vector.");

  R_xlen_t i, len = xlength(string);
  SEXP res = PROTECT(allocVector(INTSXP, len));

  for(i = 0; i < len; ++i) {
    // It would be nice to be able to skip the STRING_ELT stuff and access the
    // data directly as we do, but don't know how to get the size of
    // SEXPREC_ALIGN directly.

    unsigned const char * char_start, * char_ptr;
    unsigned char char_val;

    SEXP char_cont = STRING_ELT(string, i);
    cetype_t char_enc = getCharCE(char_cont);
    char_start = as_utf8_char(char_cont);

    int byte_count = 0, char_count = 0;
    int too_long = 0; // track if any strings longer than INT_MAX

    while((char_val = *(char_ptr = (char_start + byte_count)))) {
      int byte_off = abs(char_offset(char_ptr, char_enc == CE_BYTES));
      if((byte_count > INT_MAX - byte_off) && !too_long) {
        // nocov start
        too_long = 1;
        warning("Some elements longer than INT_MAX, return NA for those.");
        break;
        // nocov end
      }
      byte_count += byte_off;
      char_count++;
    }
    INTEGER(res)[i] = too_long ? NA_INTEGER : char_count;
  }
  UNPROTECT(1);
  return(res);
}
/*
 * Compute byte lenght of each UTF8 character
 *
 * For testing purposes only.
 *
 * @param string scalar character
 * @return integer vector containing byte length of each character, with
 *   negative values for invalid UTF8 sequences
 */
SEXP CSR_char_offsets(SEXP string) {
  if(TYPEOF(string) != STRSXP)
    error("Argument `string` must be a character vector.");
  R_xlen_t len = xlength(string);
  if(len != 1) error("Argument `string` must be scalar.");

  unsigned const char * char_start, * char_ptr;
  unsigned char char_val;

  SEXP chr_cont = STRING_ELT(string, 0);
  cetype_t char_enc = getCharCE(chr_cont);
  char_start = as_utf8_char(chr_cont);

  // won't have more chars than bytes in the translated string, we might
  // overallocate here (but drop the extra at the end)
  int * char_offs =
    (int *) R_alloc(strlen((const char *) char_start), sizeof(int));

  int byte_count = 0, char_count = 0;

  while((char_val = *(char_ptr = (char_start + byte_count)))) {
    int byte_off = char_offset(char_ptr, char_enc == CE_BYTES);
    if((byte_count > INT_MAX - abs(byte_off))) {
      // nocov start
      error("Internal Error: string has more than INT_MAX bytes.");
      break;
      // nocov end
    }
    byte_count += abs(byte_off);
    char_offs[char_count] = byte_off;
    char_count++;
  }
  SEXP res = PROTECT(allocVector(INTSXP, char_count));
  for(int i = 0; i < char_count; ++i) INTEGER(res)[i] = char_offs[i];
  UNPROTECT(1);
  return(res);
}
