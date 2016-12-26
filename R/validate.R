#' Validate Objects and Function Arguments
#'
#' Use templates to validate objects or function arguments. \code{validate}
#' validates objects, and \code{validate_args} can be used inside a function
#' to validate the function arguments.
#'
#' The \code{validate} functions use R objects as templates that the values
#' being tested must match.  For example, in:\preformatted{
#' validate(numeric(), 3.145)
#' validate(numeric(), c(1, 2, 3))
#' validate(numeric(), matrix(runif(9), 3))
#' }
#' the first argument is the template, and the second the value to check.
#' \code{validate} uses whatever aspects of the template are defined and
#' ensures that the value tested
#'
#' Each argument to \code{validate_args} is matched to one argument of the
#' enclosing function following the same rules \code{\link{match.call}} uses.
#' For example, in:
#' \preformatted{
#' function(a, b) {
#'   validate_args(numeric(), logical(1L))
#' }
#' }
#' \code{numeric()} will be used to validate_args \code{a}, and
#' \code{logical(1L)} to validate_args \code{b}.
#'
#' The default validation mechanism is to use the arguments to
#' \code{validate_args} as templates.  For example, \code{numeric()} means
#' arguments to \code{a} must be numeric, of any length, and \code{logical(1L)}
#' means arguments to \code{b} must be one length logical.  Arguments must
#' match the template structure, but need not match the values.
#' \code{validate_args} uses \code{alike} to determine whether an object
#' matches a template.
#'
#' You may use \code{||} and/or \code{&&} to construct more complex
#' validations: \preformatted{
#' function(a, b) {
#'   validate_args(
#'     a=numeric() || character(),
#'     b=character(1L) || NULL
#'   )
#' }
#' }
#' \code{validate_args} parses each expression to isolate the templates and then
#' tests each template in turn verifying that any/all of them match the
#' arguments as specified by you with \code{&&} / \code{||}.
#'
#' Often it is useful to be able to use arbitrary expressions as part of a
#' validation.  You may do so by using \code{.()}.  Any expression within
#' \code{.()} will be evaluated as is, and an argument will validate_args
#' successfully if that expression returns \code{TRUE} (note, a two length
#' logical like \code{c(T, T)} will fail):
#' \preformatted{
#' function(a, b) {
#'   validate_args(
#'     a=numeric() && .(!any(is.na(.))),
#'     b=logical() && .(length(a) == length(.) && !any(is.na(.)))
#'   )
#' }
#' }
#' If you use \code{.} as a variable anyplace in the validation expression it
#' will be substituted by the corresponding argument name prior to validation.
#' If you need to use objects called \code{.} or \code{.()} in your validation
#' expression you may do so by escaping them with another period (e.g. use
#' \code{..} for a literal \code{.}).
#'
#' If you find yourself using particular custom validation expressions
#'
#' @useDynLib validate, .registration=TRUE, .fixes="VALC_"
#' @note Will force evaluation of any arguments that are being checked (you may
#'   omit arguments that should not be evaluate from \code{validate_args})
#' @name validate
#' @rdname validate
#' @aliases validate_args
#' @export
#' @param ... arguments to validate_args; they will be matched to the enclosing
#'   function formals as with \code{match.call}
#' @param a template expression
#' @param current a value to validate
#' @param return.mode character(1L), controls the format of the return value for
#'   \code{validate}, in case of failure.  One of:\itemize{
#'     \item "text": (default) character(1L) message for use elsewhere in code
#'     \item "full": character(1L) the full error message used in "stop" mode,
#'       but actually returned instead of thrown as an error
#'     \item "raw": character(N) least processed version of the error message
#'       with none of the formatting or surrounding verbiage
#' }
#' @param stop logical(1L) whether to call \code{\link{stop}} on failure
#'   (default) or not
#' @return TRUE if validation succeeds, otherwise \code{stop} for
#'   \code{validate_args} and varies for \code{validate} according to value
#'   chosen in \code{return.mode}

validate_args <- function(...)
  .Call(VALC_validate_args, sys.frames(), sys.calls(), sys.parents())

# Do we need both `substitute(target)` and sys.call??

#' @rdname validate
#' @export

validate <- function(target, current, format="text", stop=TRUE)
  .Call(
    VALC_validate, substitute(target), current, substitute(current), 
    sys.call(), parent.frame(), format, stop
  )

#' @export

stopifnotvalid <- function(target, current, envir=parent.frame()) {
  res <- .Call(VALC_validate, target, current, envir)
  if(isTRUE(res)) return(res)
  stop(res)
}
