#' validate_args Function Arguments
#'
#' validate_args function arguments by providing templates and expressions to match
#' to the arguments of an enclosing function.
#'
#' Each argument to \code{validate_args} is matched to one argument of the enclosing
#' function following the same rules \code{\link{match.call}} uses.  For
#' example, in:
#' \preformatted{
#' function(a, b) {
#'   validate_args(numeric(), logical(1L))
#' }
#' }
#' \code{numeric()} will be used to validate_args \code{a}, and
#' \code{logical(1L)} to validate_args \code{b}.
#'
#' The default validation mechanism is to use the arguments to \code{validate_args}
#' as templates.  For example, \code{numeric()} means arguments to \code{a}
#' must be numeric, of any length, and \code{logical(1L)} means arguments to
#' \code{b} must be one length logical.  Arguments must match the template
#' structure, but need not match the values.  \code{validate_args} uses
#' \code{alike} to determine whether an object matches a template.
#'
#' You may use \code{||} and/or \code{&&} to construct more complex
#' validations:
#' \preformatted{
#' function(a, b) {
#'   validate_args(
#'     a=numeric() || character(),
#'     b=character(1L) || NULL
#' }
#' }
#' \code{validate_args} parses each expression to isolate the templates and then
#' tests each template in turn verifying that any/all of them match the
#' arguments as specified by you with \code{&&} / \code{||}.
#'
#' Often it is useful to be able to use arbitrary expressions as part of a
#' validation.  You may do so by using \code{.(}.  Any expression within
#' \code{.(} will be evaluated as is, and an argument will validate_args
#' successfully if that expression returns \code{TRUE} (note, a two length
#' logical like \code{c(T, T)} will fail):
#' \preformatted{
#' function(a, b) {
#'   validate_args(
#'     a=numeric() && .(!any(is.na(.))),
#'     b=logical() && .(length(a) == length(.) && !any(is.na(.)))
#' }
#' }
#' If you use \code{.} as a variable anyplace in the validation expression it
#' will be substituted by the corresponding argument name prior to validation.
#' If you need to use objects called \code{.} or \code{.(} in your validation
#' expression you may do so by escaping them with another period (e.g. use
#' \code{..} for a literal \code{.}).
#'
#' @useDynLib validate, .registration=TRUE, .fixes="VALC_"
#' @note Will force evaluation of any arguments that are being checked (you may
#'   omit arguments that should not be evaluate from \code{validate_args})
#' @export
#' @param ... arguments to validate_args; they will be matched to the enclosing
#'   function formals as with \code{match.call}
#' @return TRUE invisibly if validation succeeds, throws an error with
#'   \code{\link{stop}} otherwise

validate_args <- function(...)
  invisible(.Call(VALC_validate_args, sys.frames(), sys.calls(), sys.parents()))

#' @export

validate <- function(target, current)
  .Call(VALC_validate, substitute(target), current, sys.call(), sys.frame(sys.nframe()))

#' @export

stopifnotvalid <- function(target, current, envir=parent.frame()) {
  res <- .Call(VALC_validate, target, current, envir)
  if(isTRUE(res)) return(res)
  stop(res)
}
