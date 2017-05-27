#' Vet Objects and Function Arguments
#'
#' Use templates and expressions to vet objects or function arguments.
#' \code{vet} vets objects, and \code{vetr} vets the formals of the
#' function that encloses it.
#'
#' See \code{vignette('vetr', package='vetr')} and examples for details on how
#' to use these functions.
#'
#' @useDynLib vetr, .registration=TRUE, .fixes="VALC_"
#' @note Will force evaluation of any arguments that are being checked (you may
#'   omit arguments that should not be evaluate from \code{vetr})
#' @name vet
#' @rdname vet
#' @aliases vetr tev
#' @export
#' @seealso \code{\link{alike}} for how templates are used
#' @param ... arguments to vetr; they will be matched to the enclosing
#'   function formals as with \code{\link{match.call}}
#' @param target a template, a vetting expression, or a compound expression
#' @param current an object to vet
#' @param format character(1L), controls the format of the return value for
#'   \code{vet}, in case of failure.  One of:\itemize{
#'     \item "text": (default) character(1L) message for use elsewhere in code
#'     \item "full": character(1L) the full error message used in "stop" mode,
#'       but actually returned instead of thrown as an error
#'     \item "raw": character(N) least processed version of the error message
#'       with none of the formatting or surrounding verbiage
#' }
#' @param stop TRUE or FALSE whether to call \code{\link{stop}} on failure
#'   (default) or not
#' @return TRUE if validation succeeds, otherwise \code{stop} for
#'   \code{vetr} and varies for \code{vet} according to value chosen in
#'   \code{stop}
#' @examples
#' ## template vetting
#' vet(numeric(2L), runif(2))
#' vet(numeric(2L), runif(3))
#' vet(numeric(2L), letters)
#' vet(numeric(2L), letters, stop=TRUE)
#'
#' ## Zero length templates are wild cards
#' vet(numeric(), runif(2))
#' vet(numeric(), runif(100))
#' vet(numeric(), letters)
#'
#' ## Short (<100 length) integer-like numerics will
#' ## pass for integer
#' vet(integer(), c(1, 2, 3))
#' vet(integer(), c(1, 2, 3) + 0.1)
#'
#' ## Nested templates; note, in packages you should consider
#' ## defining templates outside of `vet` or `vetr` so that
#' ## they are computed on load rather that at runtime
#' vet(
#'   list(numeric(1L), matrix(integer(), 3)),
#'   list(runif(1), rbind(1:10, 1:10, 1:10))
#' )
#' vet(
#'   list(numeric(1L), matrix(integer(), 3)),
#'   list(runif(1), cbind(1:10, 1:10, 1:10))
#' )
#'
#' ## Vetting expression
#' vet(. > 0, runif(10))
#' vet(. > 0, -runif(10))
#'
#' ## Compound expressions (template + expression(s)):
#' vet(numeric(2L) && . > 0, runif(2))
#' vet(numeric(2L) && . > 0, runif(10))
#' vet(numeric(2L) && . > 0, -runif(2))
#'
#' ## Using pre-defined tokens
#' vet(INT.1, 1)

vetr <- function(...)
  .Call(
    VALC_validate_args,
    fun.match <- sys.function(sys.parent(1)),
    match.call(
      definition=fun.match,
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

