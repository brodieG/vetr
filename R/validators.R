# Copyright (C) 2017  Brodie Gaslam
#
# This file is part of "vetr - Trust, but Verify"
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

#' Vetting Tokens With Custom Error Messages
#'
#' Utility function to generate vetting tokens with attached error messages.
#' You should only need to use this if the error message produced naturally by
#' `vetr` is unclear.  Several predefined tokens created by this function
#' are also documented here.
#'
#' Allows you to supply error messages for vetting to use for each error
#' token.  Your token should not contain top level \code{&&} or \code{||}.  If
#' it does your error message will not be reported because `vetr` looks for
#' error messages attached to atomic tokens.  If your token must involve
#' top level \code{&&} or `||`, use \code{I(x && y)} to ensure that
#' your error message is used by `vet`, but beware than in doing so you do
#' not use templates within the `I` call as everything therein will be
#' interpreted as a vetting expression rather than a template.
#'
#' Error messages are typically of the form \dQuote{\%sshould be XXX}.
#'
#' This package ships with many predefined tokens for common use cases. They
#' are listed in the \dQuote{Usage} section of this documentation.  The tokens
#' are named in format `TYPE[.LENGTH][.OTHER]`.  For example
#' `INT` will vet an integer vector, `INT.1` will vet a scalar integer
#' vector, and `INT.1.POS.STR` will vet a strictly positive integer vector.
#' At this time tokens are predefined for the basic types as scalars or
#' any-length vectors.  Some additional checks are available (e.g. positive only
#' values).
#'
#' Every one of the predefined vetting tokens documented here implicitly
#' disallows NAs.  Numeric tokens also disallow infinite values. If you wish
#' to allow NAs or infinite values just use a template object (e.g.
#' `integer(1L)`).
#'
#' @note **This will only work with standard tokens containing `.`**.  Anything
#' else will be interpreted as a template token.
#'
#' @export
#' @seealso [vet()]
#' @param exp an expression which will be captured but not evaluated
#' @param err.msg character(1L) a message that tells the user what the
#'   expected value should be, should contain a \dQuote{\%s} for `sprintf`
#'   to use (e.g. \dQuote{\%sshould be greater than 2})
#' @return a quoted expressions with `err.msg` attribute set
#' @examples
#' ## Predefined tokens:
#' vet(INT.1, 1:2)
#' vet(INT.1 || LGL, 1:2)
#' vet(INT.1 || LGL, c(TRUE, FALSE))
#'
#' ## Check squareness
#' mx <- matrix(1:3)
#' SQR <- vet_token(nrow(.) == ncol(.), "%sshould be square")
#' vet(SQR, mx)
#'
#' ## Let `vetr` make up error message; note `quote` vs `vet_token`
#' ## Often, `vetr` does fine without explictly specified err msg:
#' SQR.V2 <- quote(nrow(.) == ncol(.))
#' vet(SQR.V2, mx)
#'
#' ## Combine some tokens, notice how we use `quote` at the combining
#' ## step:
#' NUM.MX <- vet_token(matrix(numeric(), 0, 0), "%sshould be numeric matrix")
#' SQR.NUM.MX <- quote(NUM.MX && SQR)
#' vet(SQR.NUM.MX, mx)
#'
#' ## If instead we used `vet_token` the overall error message
#' ## is not used; instead it falls back to the error message of
#' ## the specific sub-token that fails:
#' NUM.MX <- vet_token(matrix(numeric(), 0, 0), "%sshould be numeric matrix")
#' SQR.NUM.MX.V2 <-
#'   vet_token(NUM.MX && SQR, "%sshould be a square numeric matrix")
#' vet(SQR.NUM.MX.V2, mx)

vet_token <- function(exp, err.msg="%s") {
  if(
    !is.character(err.msg) || length(err.msg) != 1L || is.na(err.msg) ||
    inherits(try(sprintf(err.msg, "test"), silent=TRUE), "try-error") ||
    identical(sprintf(err.msg, "test"), err.msg)
  ) {
    stop(
      "Argument `err.msg` must be character(1L) and contain a single '%s' ",
      "for use by `sprintf`."
    )
  }
  x <- substitute(exp)
  attr(x, "err.msg") <- err.msg
  x
}
#' @rdname vet_token
#' @export

NO.NA <- vet_token(!is.na(.), "%sshould not contain NAs, but does")

#' @export
#' @name vet_token

NO.INF <- vet_token(
  is.finite(.), "%sshould contain only finite values, but does not"
)
#' @export
#' @name vet_token

GTE.0 <- vet_token(
  . >= 0, "%sshould contain only positive values, but has negatives"
)

#' @export
#' @name vet_token

LTE.0 <- vet_token(
  . <= 0, "%sshould contain only negative values, but has positives"
)

#' @export
#' @name vet_token

GT.0 <- vet_token(
  . > 0,
  paste0(
    "%sshould contain only \"strictly positive\" values, but has zeroes or ",
    "negatives"
  )
)

#' @export
#' @name vet_token

LT.0 <- vet_token(
  . < 0,
  paste0(
    "%sshould contain only \"strictly negative\" values, but has zeroes ",
    "or positives"
  )
)

#' @export
#' @name vet_token

INT.1 <- quote(integer(1L) && NO.NA && NO.INF)

#' @export
#' @name vet_token

INT.1.POS <- quote(integer(1L) && NO.NA && NO.INF && GTE.0)

#' @export
#' @name vet_token

INT.1.NEG <- quote(integer(1L) && NO.NA && NO.INF && LTE.0)

#' @export
#' @name vet_token

INT.1.POS.STR <- quote(integer(1L) && NO.NA && NO.INF && GT.0)

#' @export
#' @name vet_token

INT.1.NEG.STR <- quote(integer(1L) && NO.NA && NO.INF && LT.0)

#' @export
#' @name vet_token

INT <- quote(integer() && NO.NA && NO.INF)
#' @export
#' @name vet_token

INT.POS <- quote(integer() && NO.NA && NO.INF && GTE.0)

#' @export
#' @name vet_token

INT.NEG <- quote(integer() && NO.NA && NO.INF && LTE.0)

#' @export
#' @name vet_token

INT.POS.STR <- quote(integer() && NO.NA && NO.INF && GT.0)

#' @export
#' @name vet_token

INT.NEG.STR <- quote(integer() && NO.NA && NO.INF && LT.0)

#' @export
#' @name vet_token

NUM.1 <- quote(numeric(1L) && NO.NA && NO.INF)

#' @export
#' @name vet_token

NUM.1.POS <- quote(numeric(1L) && NO.NA && NO.INF && GTE.0)

#' @export
#' @name vet_token

NUM.1.NEG <- quote(numeric(1L) && NO.NA && NO.INF && LTE.0)

#' @export
#' @name vet_token

NUM <- quote(numeric() && NO.NA && NO.INF)

#' @export
#' @name vet_token

NUM.POS <- quote(numeric() && NO.NA && NO.INF && GTE.0)

#' @export
#' @name vet_token

NUM.NEG <- quote(numeric() && NO.NA && NO.INF && LTE.0)

#' @export
#' @name vet_token

CHR.1 <- quote(character(1L) && NO.NA)

#' @export
#' @name vet_token

CHR <- quote(character() && NO.NA)

#' @export
#' @name vet_token

CPX <- quote(complex() && NO.NA && NO.INF)

#' @export
#' @name vet_token

CPX.1 <- quote(complex(1L) && NO.NA && NO.INF)

#' @export
#' @name vet_token

LGL <- quote(logical() && NO.NA)

#' @export
#' @name vet_token

LGL.1 <- quote(logical(1L) && NO.NA)


