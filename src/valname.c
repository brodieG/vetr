#include <R.h>
#include <Rinternals.h>
#include "alike.h"
#include <wctype.h>

// extern int R_Is_Running;
extern Rboolean mbcslocale;
/* A version that reports failure as an error */
/*
 * Taken and adapted from R 3.2.2 src/main/util.c@1324
 */
size_t Mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps)
{
  size_t used;

  if(n <= 0 || !*s) return (size_t)0;
  used = mbrtowc(wc, s, n, ps);
  if((int) used < 0) {
    /* This gets called from the menu setup in RGui */
    // if (!R_Is_Running) return (size_t)-1;
    /* let's try to print out a readable version */
    error("Internal Error: invalid multibyte string at");  // nocov
  }
  return used;
}
/*
 * Taken and adapted from R 3.2.2 src/main/gram.c@4915
 */
int ALIKEC_is_valid_name(const char *name)
{
  const char *p = name;
  int i;

  if(mbcslocale) {
    /* the only way to establish which chars are alpha etc is to
       use the wchar variants */
    size_t n = strlen(name), used;
    wchar_t wc;
    used = Mbrtowc(&wc, p, n, NULL);
    if((int) used <= 0) return 0;
    p += used; n -= used;
    if (wc != L'.' && !iswalpha(wc) ) return 0;
    if (wc == L'.') {
      /* We don't care about other than ASCII digits */
      if(isdigit(0xff & (int)*p)) return 0;
      /* Mbrtowc(&wc, p, n, NULL); if(iswdigit(wc)) return 0; */
    }
    while((int)(used = Mbrtowc(&wc, p, n, NULL)) > 0) {
      if (!(iswalnum(wc) || wc == L'.' || wc == L'_')) break;
      p += used; n -= used;
    }
    if (*p != '\0') return 0;
  } else {
    // nocov start current local has MB_CUR_MAX > 1, so this never runs
    int c = 0xff & *p++;
    if (c != '.' && !isalpha(c) ) return 0;
    if (c == '.' && isdigit(0xff & (int)*p)) return 0;
    while ( c = 0xff & *p++, (isalnum(c) || c == '.' || c == '_') ) ;
    if (c != '\0') return 0;
    // nocov end
  }

  if (strcmp(name, "...") == 0) return 1;

  const char * keywords[20] = {
    "NULL", "NA", "TRUE", "FALSE", "Inf", "NaN", "NA_integer_", "NA_real_",
    "NA_character_", "NA_complex_", "function", "while", "repeat", "for",
    "if", "in", "else", "next", "break", "..."
  };
  for (i = 0; i < 20; i++)
    if (strcmp(keywords[i], name) == 0) return 0;

  return 1;
}
/*
 * External version for testing
 */
SEXP ALIKEC_is_valid_name_ext(SEXP name) {
  if(TYPEOF(name) != STRSXP || XLENGTH(name) != 1)
    error("Argument `name` must be character(1L)");
  return ScalarLogical(ALIKEC_is_valid_name(CHAR(asChar(name))));
}
