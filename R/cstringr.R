
#' Number of Characters Taken By Numeric Representation
#'
#' e.g., for 100, would be 3, for 1324, would be 4.
#'
#' @param x integer(1L)
#' @return integer(1L)
#' @export

len_chr_len <- function(x) .Call(CSR_len_chr_len_ext, x)

#' Number Converted To character
#'
#' e.g., \code{100} becomes \code{"100"}
#'
#' @param x integer(1L)
#' @return character(1L)
#' @export

len_as_chr <-function(x) .Call(CSR_len_as_chr_ext, x)

#' "Safe" String Manipulation Functions
#'
#' Implements variants of \code{strlcpy}, \code{strnlen}, etc.
#'
#' These functions are intended to be used directly from C but are exposed here
#' for testing purposes.  They differ from similar existing versions in these
#' respects:
#'
#' \itemize{
#'   \item Input strings are never modified; if changes are required to comply
#'     with length limits a copy that is \code{R_alloc}ed is returned
#'   \item Only C99 code is used
#' }
#' Function specific details follow.  Pay attention, the interfaces are not
#' exactly the same as the \code{C} functions they intend to replace.  For
#' example \code{smprintf} does not have a \code{str} parameter like
#' \code{snprintf}, and \code{strmcpy} returns a new character string instead of
#' modifying one passed as an argument.).
#'
#' \itemize{
#'   \item \code{strmlen}: roughly equivalent to POSIX \code{strnlen}, but
#'     should be fully portable
#'   \item \code{strmcpy}: if \code{strmlen(str)} is greater than \code{maxlen},
#'     returns a copy of \code{str} with \code{'\0'} as the \code{maxlen + 1}th
#'     character (note return character array is not allocated beyond this NULL),
#'     otherwise returns a copy of \code{str}.
#'   \item \code{smprintf2}: returns a newly \code{R_alloc}ed string that is just
#'     large enough to fit the combination of the format string and the tokens
#'     to sub in (note that tokens must be character so only "%s" is meaningful
#'     as a formatting element).  To achieve this without overflow, ensures that
#'     \code{format} and each token is limited to \code{maxlen - 1} characters.
#'     Fails if result string is longer than a \code{size_t} could index.  Note
#'     that internally we define \code{smprintf1} - \code{smprintf6}, but we are
#'     only exporting the one function directly to R for testing.
#'   \item \code{strbullet} ads a bullet at front of string and padding after
#'     each new line
#'   \item \code{collapse} is essentially the same as \code{paste0(x,
#'     collapse=sep}
#' }
#' @export
#' @aliases strmcpy smprintf2 ucfirst lcfirst strbullet collapse
#' @param str character string to measure or manipulate, should be scalar for
#'   \code{strmlen} and \code{strmcpy}
#' @param format character(1L) string to use as format template
#' @param maxlen integer(1L) size limit to truncate to, or to limit tokens to
#' @param a character(1L) another string
#' @param b character(1L) another string
#' @param bullet character(1L) a string to use as the bullet symbol
#' @param ctd character(1L) a string preferably the same number of characters as
#'   \code{bullet} to use after the first line wraps in a bullet
#' @param sep character(1L) a separator to use when collapsing character vectors
#'   with \code{collapse}
#' @return integer(1L) for \code{strmlen}, character(1L) for \code{strmcpy}
#'   and \code{smprintf2}

strmlen <- function(str, maxlen=10000L) .Call(CSR_strmlen_ext, str, maxlen)

#' @rdname strmlen
#' @export

strmcpy <- function(str, maxlen=10000L) .Call(CSR_strmcpy_ext, str, maxlen)

#' @rdname strmlen
#' @export

smprintf2 <- function(format, a, b, maxlen=10000L)
  .Call(CSR_smprintf2_ext, maxlen, format, a, b)

#' @rdname strmlen
#' @export

ucfirst <- function(str, maxlen=10000L) .Call(CSR_ucfirst_ext, str, maxlen)

#' @rdname strmlen
#' @export

lcfirst <- function(str, maxlen=10000L) .Call(CSR_lcfirst_ext, str, maxlen)

#' @rdname strmlen
#' @export

strbullet <- function(str, bullet="- ", ctd="  ", maxlen=10000L)
  .Call(CSR_bullet_ext, str, bullet, ctd, maxlen)

#' @rdname strmlen
#' @export

collapse <- function(str, sep="", maxlen=10000L)
  .Call(CSR_collapse_ext, str, sep, maxlen)

## Purely internal funs for testing

test1 <- function() .Call(CSR_test_strmcpy)
test2 <- function() .Call(CSR_test_strappend)
test3 <- function() .Call(CSR_test_add_szt)

test4 <- function() .Call(CSR_test_smprintfx)

test5 <- function() .Call(CSR_test_strappend2)

