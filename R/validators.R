#' Make Validator Token
#'
#' Utility function generate validator tokens.
#'
#' Allows you to supply error messages for validator to use for each error
#' token.  Your token should not contain top level \code{&&} or \code{||}.  If
#' it does your error message will not be reported.  If your token must involve
#' top level \code{&&} or \code{||}, use \code{identity(x && y)} to ensure that
#' your error message is used by .
#'
#' @export
#' @param exp an expression which will be captured but not evaluated
#' @param err.msg char
#' @return a quoted expressions with \code{err.msg} attribute set
#' @examples
#' LEN.2 <- mk_val_token(length(.) == 2, "is not length two")

mk_val_token <- function(exp, err.msg="") {
  if(!is.character(err.msg)) stop("Argument `err.msg` must be character.")
  x <- substitute(exp)
  attr(x, "err.msg") <- err.msg
  x
}
#' Sub Validator Tokens
#'
#' Validation tokens used in more complex validator expressions.
#'
#' @aliases NO.INF
#' @seealso \code{\link{validator_atomic}}
#' @export
#' @rdname validator_sub

NO.NA <- mk_val_token(!is.na(.), "contains NAs")

#' @export
#' @rdname validator_sub

NO.INF <- mk_val_token(!is.finite(.), "contains infinite values")

#' Atomic Vector validator
#'
#' Validate against a specific atomic vector type of any length, or of length 1
#' (i.e. scalars) that contain no NAs and are finite if inifinity is an option.
#'
#' If you wish to allow NAs or infinite values just use \code{integer(1L)} or
#' some such as a template.
#'
#' @aliases NUM1 CHR1 LGL1 CPX1 INT NUM CHR CPX LGL
#' @seealso \code{\link{validator_sub}}
#' @export
#' @rdname validator_atomic

INT1 <- quote(integer(1L) && NO.NA && NO.INF)  # need finite because we allow integer-like
#' @export
#' @rdname validator_atomic
NUM1 <- quote(numeric(1L) && NO.NA && NO.INF)
#' @export
#' @rdname validator_atomic
CPX1 <- quote(complex(1L) && NO.NA && NO.INF)
#' @export
#' @rdname validator_atomic
CHR1 <- quote(character(1L) && NO.NA)
#' @export
#' @rdname validator_atomic
LGL1 <- quote(logical(1L) && NO.NA)

#' @export
#' @rdname validator_atomic
LGL <- quote(logical() && NO.NA)
#' @export
#' @rdname validator_atomic
INT <- quote(integer() && NO.NA && NO.INF)
#' @export
#' @rdname validator_atomic
NUM <- quote(numeric() && NO.NA && NO.INF)
#' @export
#' @rdname validator_atomic
CHR <- quote(character() && NO.NA)
#' @export
#' @rdname validator_atomic
CPX <- quote(complex() && NO.NA && NO.INF)
