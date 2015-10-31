#' Make Validator Token
#'
#' Utility function to generate validator tokens.
#'
#' Allows you to supply error messages for validator to use for each error
#' token.  Your token should not contain top level \code{&&} or \code{||}.  If
#' it does your error message will not be reported.  If your token must involve
#' top level \code{&&} or \code{||}, use \code{identity(x && y)} to ensure that
#' your error message is used by \code{validate}.
#'
#' Error messages should tell you what the acceptable values are so they mesh
#' with the rest of the error reporting messages.  Typically, we will append
#' "Should" ahead of your message, such that if you supply "not contain NAs"
#' it will be displayed as "Should not contain NAs".
#'
#' @export
#' @param exp an expression which will be captured but not evaluated
#' @param err.msg character(1L) a message that tells the user what the
#'   expected value should be
#' @return a quoted expressions with \code{err.msg} attribute set
#' @examples
#' LEN.2 <- mk_val_token(length(.) == 2, "be length two")

mk_val_token <- function(exp, err.msg="") {
  if(!is.character(err.msg)) stop("Argument `err.msg` must be character.")
  x <- substitute(exp)
  attr(x, "err.msg") <- err.msg
  x
}
#' Predefined Validation Tokens
#'
#' Commonly used tokens that can be used as part of validation expressions.
#'
#' In particular, we predefine several templates useful for atomic vectors. For
#' example, \code{INT.1.POS.STR} will validate:
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
#' @name validation_tokens
#' @rdname validation_tokens
#' @export

NO.NA <- mk_val_token(!is.na(.), "not contain NAs, but does")

#' @export
#' @name validation_tokens

NO.INF <- mk_val_token(is.finite(.), "contain only finite values, but does not")

#' @export
#' @name validation_tokens

GTE.0 <- mk_val_token(. >= 0, "contain only positive values, but has negatives")

#' @export
#' @name validation_tokens

LTE.0 <- mk_val_token(. <= 0, "contain only negative values, but has positives")

#' @export
#' @name validation_tokens
GT.0 <- mk_val_token(. > 0, "contain only \"strictly positive\" values, but has zeroes or negatives")
#' @export
#' @name validation_tokens
LT.0 <- mk_val_token(. < 0, "contain only \"strictly negative\" values, but has zeroes or positives")

#' Atomic Vector validator
#'
#'
#' @aliases NUM1 CHR1 LGL1 CPX1 INT NUM CHR CPX LGL
#' @seealso \code{\link{validator_sub}}
#' @export
#' @rdname validator_atomic

#' @export
#' @name validation_tokens

INT.1 <- quote(integer(1L) && NO.NA && NO.INF)
#' @export
#' @name validation_tokens

INT.1.POS <- quote(integer(1L) && NO.NA && NO.INF && GTE.0)

#' @export
#' @name validation_tokens

INT.1.NEG <- quote(integer(1L) && NO.NA && NO.INF && LTE.0)

#' @export
#' @name validation_tokens

INT.1.POS.STR <- quote(integer(1L) && NO.NA && NO.INF && GT.0)

#' @export
#' @name validation_tokens

INT.1.NEG.STR <- quote(integer(1L) && NO.NA && NO.INF && LT.0)

#' @export
#' @name validation_tokens

INT <- quote(integer() && NO.NA && NO.INF)
#' @export
#' @name validation_tokens

INT.POS <- quote(integer() && NO.NA && NO.INF && GTE.0)

#' @export
#' @name validation_tokens

INT.NEG <- quote(integer() && NO.NA && NO.INF && LTE.0)

#' @export
#' @name validation_tokens

INT.POS.STR <- quote(integer() && NO.NA && NO.INF && GT.0)

#' @export
#' @name validation_tokens

INT.NEG.STR <- quote(integer() && NO.NA && NO.INF && LT.0)

#' @export
#' @name validation_tokens

NUM.1 <- quote(numeric(1L) && NO.NA && NO.INF)

#' @export
#' @name validation_tokens

NUM.1.POS <- quote(numeric(1L) && NO.NA && NO.INF && GTE.0)

#' @export
#' @name validation_tokens

NUM.1.NEG <- quote(numeric(1L) && NO.NA && NO.INF && LTE.0)

#' @export
#' @name validation_tokens

NUM <- quote(numeric() && NO.NA && NO.INF)

#' @export
#' @name validation_tokens

NUM.POS <- quote(numeric() && NO.NA && NO.INF && GTE.0)

#' @export
#' @name validation_tokens

NUM.NEG <- quote(numeric() && NO.NA && NO.INF && LTE.0)

#' @export
#' @name validation_tokens

CHR.1 <- quote(character(1L) && NO.NA)

#' @export
#' @name validation_tokens

CHR <- quote(character() && NO.NA)

#' @export
#' @name validation_tokens

CPX <- quote(complex() && NO.NA && NO.INF)

#' @export
#' @name validation_tokens

CPX.1 <- quote(complex(1L) && NO.NA && NO.INF)

#' @export
#' @name validation_tokens

LGL <- quote(logical() && NO.NA)

#' @export
#' @name validation_tokens

LGL.1 <- quote(logical(1L) && NO.NA)


