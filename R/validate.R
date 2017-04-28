#' Vet Objects and Function Arguments
#'
#' Use templates and expressions to vet objects or function arguments.
#' \code{vetq} vets objects, and \code{vetq_args} vets the formals of the
#' function it is enclosed in.
#'
#' @section Vetting with Templates:
#'
#'
#'
#'
#' The \code{vet} functions use R objects as templates that the values
#' being tested must match.  For example, in:\preformatted{
#' vet(numeric(), 3.145)
#' vet(numeric(), c(1, 2, 3))
#' vet(numeric(), matrix(runif(9), 3))
#' }
#' the first argument is the template, and the second the value to check.
#' \code{vet} uses whatever aspects of the template are defined and
#' ensures that the value tested
#'
#' Each argument to \code{vetr} is matched to one argument of the
#' enclosing function following the same rules \code{\link{match.call}} uses.
#' For example, in:
#' \preformatted{
#' function(a, b) {
#'   vetr(numeric(), logical(1L))
#' }
#' }
#' \code{numeric()} will be used to vetr \code{a}, and
#' \code{logical(1L)} to vetr \code{b}.
#'
#' The default validation mechanism is to use the arguments to
#' \code{vetr} as templates.  For example, \code{numeric()} means
#' arguments to \code{a} must be numeric, of any length, and \code{logical(1L)}
#' means arguments to \code{b} must be one length logical.  Arguments must
#' match the template structure, but need not match the values.
#' \code{vetr} uses \code{alike} to determine whether an object
#' matches a template.
#'
#' You may use \code{||} and/or \code{&&} to construct more complex
#' validations: \preformatted{
#' function(a, b) {
#'   vetr(
#'     a=numeric() || character(),
#'     b=character(1L) || NULL
#'   )
#' }
#' }
#' \code{vetr} parses each expression to isolate the templates and then
#' tests each template in turn verifying that any/all of them match the
#' arguments as specified by you with \code{&&} / \code{||}.
#'
#' Often it is useful to be able to use arbitrary expressions as part of a
#' validation.  You may do so by using \code{.()}.  Any expression within
#' \code{.()} will be evaluated as is, and an argument will vetr
#' successfully if that expression returns \code{TRUE} (note, a two length
#' logical like \code{c(T, T)} will fail):
#' \preformatted{
#' function(a, b) {
#'   vetr(
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
#' @section Validation Expressions:
#'
#' Validation expressions are subject to non-standard evaluation.  They are
#' recursively substituted until all symbols in the initially substituted call
#' that point to language objects no longer point to language.  For example, in:
#' \preformat{
#' a <- b <- c <- TRUE   # these are non-language objects
#' x <- quote(a && b)    # this is a language object
#' vet(target=x || c, current=z)
#' }
#' \code{target} will be expanded into \code{(a && b) || c}.
#'
#' @useDynLib vetr, .registration=TRUE, .fixes="VALC_"
#' @note Will force evaluation of any arguments that are being checked (you may
#'   omit arguments that should not be evaluate from \code{vetr})
#' @name vet
#' @rdname vet
#' @aliases vetr
#' @export
#' @param ... arguments to vetr; they will be matched to the enclosing
#'   function formals as with \code{match.call}
#' @param a template expression
#' @param current a value to vet
#' @param format character(1L), controls the format of the return value for
#'   \code{vet}, in case of failure.  One of:\itemize{
#'     \item "text": (default) character(1L) message for use elsewhere in code
#'     \item "full": character(1L) the full error message used in "stop" mode,
#'       but actually returned instead of thrown as an error
#'     \item "raw": character(N) least processed version of the error message
#'       with none of the formatting or surrounding verbiage
#' }
#' @param stop logical(1L) whether to call \code{\link{stop}} on failure
#'   (default) or not
#' @return TRUE if validation succeeds, otherwise \code{stop} for
#'   \code{vetr} and varies for \code{vet} according to value chosen in
#'   \code{return.mode}

vetr <- function(...)
  .Call(
    VALC_validate_args,
    match.call(
      definition=(fun.match <- sys.function(sys.parent(1))),
      call=sys.call(sys.parent(1)),
      envir=parent.frame(2L)
    ),
    match.call(definition=fun.match, envir=(par.frame <- parent.frame())),
    par.frame
  )

# Do we need both `substitute(target)` and sys.call??

#' @rdname vet
#' @export

vet <- function(target, current, format="text", stop=FALSE)
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
