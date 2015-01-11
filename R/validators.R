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

NO.NA <- mk_val_token(!is.na(.), "contains NAs, but should not")

#' @export
#' @rdname validator_sub

NO.INF <- mk_val_token(is.finite(.), "contains infinite values, but should not")

GTE.0 <- mk_val_token(. < 0, "contains negative values, but should not")
LTE.0 <- mk_val_token(. > 0, "contains positive values, but should not")

GT.0 <- mk_val_token(. <= 0, "contains non-\"strictly positive\" values, but should not")
LT.0 <- mk_val_token(. >= 0, "contains non-\"strictly negative\" values, but should not")

#' Atomic Vector validator
#'
#' Use as token to validate against atomic vectors with particular properties.
#'
#' For example, \code{INT.1.POS.STR} will validate:
#' \itemize{
#'   \item \code{INT}: integer-like
#'   \item \code{1}: length 1
#'   \item \code{POS.STR}: strictly positive (i.e. > zero)
#' }
#' whereas \code{INT} will validate any integer like vector of any length.
#'
#' Every one of the token validators documented here implicitly disallows NAs,
#' and for numerics also disallows infinite values. If you wish to allow NAs or
#' infinite values just use \code{integer(1L)}.
#'
#' Keep in mind these validators are just language objects so you can just as
#' easily create your own by quoting an R expression or by using
#' \code{\link{mk_val_token}} if you also want to attach a custom error message.
#'
#' @aliases NUM1 CHR1 LGL1 CPX1 INT NUM CHR CPX LGL
#' @seealso \code{\link{validator_sub}}
#' @export
#' @rdname validator_atomic

INT.1 <- quote(integer(1L) && NO.NA && NO.INF)  # need finite because we allow integer-like
#' @export
#' @rdname validator_atomic
INT.1.POS <- quote(integer(1L) && NO.NA && NO.INF && GTE.0)
#' @export
#' @rdname validator_atomic
INT.1.NEG <- quote(integer(1L) && NO.NA && NO.INF && LTE.0)
#' @export
#' @rdname validator_atomic
INT.1.POS.STR <- quote(integer(1L) && NO.NA && NO.INF && GT.0)
#' @export
#' @rdname validator_atomic
INT.1.NEG.STR <- quote(integer(1L) && NO.NA && NO.INF && LT.0)
#' @export
#' @rdname validator_atomic
INT <- quote(integer() && NO.NA && NO.INF)
#' @export
#' @rdname validator_atomic
INT.POS <- quote(integer() && NO.NA && NO.INF && GTE.0)
#' @export
#' @rdname validator_atomic
INT.NEG <- quote(integer() && NO.NA && NO.INF && LTE.0)
#' @export
#' @rdname validator_atomic
INT.POS.STR <- quote(integer() && NO.NA && NO.INF && GTE.0)
#' @export
#' @rdname validator_atomic
INT.NEG.STR <- quote(integer() && NO.NA && NO.INF && LTE.0)

#' @export
#' @rdname validator_atomic
NUM.1 <- quote(numeric(1L) && NO.NA && NO.INF)
#' @export
#' @rdname validator_atomic
NUM.1.POS <- quote(numeric(1L) && NO.NA && NO.INF && GTE.0)
#' @export
#' @rdname validator_atomic
NUM.1.NEG <- quote(numeric(1L) && NO.NA && NO.INF && LTE.0)
#' @export
#' @rdname validator_atomic
NUM <- quote(numeric() && NO.NA && NO.INF)
#' @export
#' @rdname validator_atomic
NUM.POS <- quote(numeric() && NO.NA && NO.INF && GTE.0)
#' @export
#' @rdname validator_atomic
NUM.NEG <- quote(numeric() && NO.NA && NO.INF && LTE.0)

#' @export
#' @rdname validator_atomic
CHR.1 <- quote(character(1L) && NO.NA)
#' @export
#' @rdname validator_atomic
CHR <- quote(character() && NO.NA)
#' @export
#' @rdname validator_atomic
CPX <- quote(complex() && NO.NA && NO.INF)
#' @export
#' @rdname validator_atomic
CPX.1 <- quote(complex(1L) && NO.NA && NO.INF)
#' @export
#' @rdname validator_atomic
LGL <- quote(logical() && NO.NA)
#' @export
#' @rdname validator_atomic
LGL.1 <- quote(logical(1L) && NO.NA)


