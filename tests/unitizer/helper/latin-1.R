# Some latin-1 codes, on own file due to Solaris issues with escaping

lat.1.1 <- lat.1.2 <- c(
  "ni\xF1a",
  "hello",
  "\xB5 \xB6 \xBF \xC9 \xF4"
)
Encoding(lat.1.1) <- "latin1"
Encoding(lat.1.2) <- "bytes"

