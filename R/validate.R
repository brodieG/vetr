#' Validate Function Arguments
#'
#' Each argument to this function (\dfn{Checkarg} hereafter) will be matched to an
#' argument of the enclosing closure (\dfn{Closarg} hereafter) in order to validate
#' it. Matching is either name based (no partial matching), positional, or a
#' combination of both, with name based matching done first, and positional
#' matching used for the remaining \dfn{Closeargs}.
#'
#' \dfn{Checkargs} fall into two categories:
#' \enumerate{
#'   \item objects, or expressions producing objects that do not reference the
#'     matching \dfn{Closarg}
#'   \item expressions that reference the matching \dfn{Closarg}
#' }
#' For example, in:
#' \preformatted{
#' function(a, b) {
#'   check_args(numeric(), nrow(b) == 3)
#' }
#' }
#' the first \dfn{Checkarg} is of Type 1 since it contains no references to \code{`a`}
#' and the second \dfn{Checkarg} is of Type 2, since it references \code{`b`}, the
#' matching \dfn{Closarg}.
#'
#' @section Type 1 \dfn{Checkargs}:
#' Type 1 \dfn{Checkargs} are taken to be templates to compare \dfn{Closargs} against.
#' These \dfn{Checkargs} are used as the \code{`obj.reference`} in a
#' \code{`\link{alike}`} comparison.  The exact nature of what is considered
#'  a valid match to a template is somewhat complex to explain, but should be
#' intuitive to grasp (see examples for \code{`\link{alike}`}).
#'
#' @section Type 2 \dfn{Checkargs}:
#' Type 2 \dfn{Checkargs} need to evaluate to TRUE in order to pass (see examples).
#'
#' @useDynLib validate, .registration=TRUE, .fixes="VALC_"
#' @note Will force evaluation of the closure's arguments that are being checked,
#'   so do not check a formal if that is a particularly undesirable side effect
#'   for that formal
#' @note checks against default values along the lines of those made with
#'   \code{`\link{match.arg}`} are not supported (this is a conscious design
#'   decision)
#' @note Hack alert: in order to get the error message to look like it came from
#'   the enclosing closure rather than this function, we use a carriage return
#'   to overwrite the first part of the error message (if you know of a better
#'   way that won't mess with try/catch handling, let me know)
#' @export
#' @param ... arguments to validate; if they are named the names must match
#'   the names of the arguments from enclosing closure (no partial matching)
#' @return NULL if validation succeeds, throws an error with \code{`\link{stop}`}
#'   otherwise

validate <- function(...)
  .Call(VALC_validate, sys.frames(), sys.calls(), sys.parents())
