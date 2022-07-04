# These examples cannot be in-line in the unitizer because Solaris fails to
# properly escape them, which then leads to failures.

# Examples from the Unicode 10.0 docs

unicode.10 <- c(
  "\xC2\x41\x41",
  "\x61\xF1\x80\x80",
  "\x61\xF1\x80\x80\xE1\x80",
  "\x61\xF1\x80\x80\xE1\x80\xC2\x62\x80\x63\x80\xBF\x64"
)
Encoding(unicode.10) <- "UTF-8"

