#' A Fuzzier Version \code{\link{typeof}}
#'
#' Numerics that are equivalent to integers (e.g \code{x == floor(x)}) are
#' classified as integers, and builtin and special functions are reported as
#' closures.
#'
#' @param object the object to check the type of
#' @return character(1L) the type of the object
#' @export
#' @examples
#'
#' type_of(1.0001)                     # numeric
#' type_of(1.0)                        # integer (`typeof` returns numeric)
#' type_of(1)                          # integer (`typeof` returns numeric)
#' type_of(sum)                        # closure (`typeof` returns builtin)
#' type_of(`$`)                        # closure (`typeof` returns special)

type_of <- function(object)
  .Call(VALC_typeof, object)

#' Compare Types of Objects
#'
#' By default, checks \code{\link{type_of}} objects and two objects are
#' considered \code{type_alike} if they have the same type.  There is special
#' handling for integers, reals, and functions.
#'
#' For integers and reals, if \code{current} is integer or integer-like
#' (e.g. 1.0) it will match real or integer \code{target} values.  Closures,
#' built-ins, and specials are all treated as type function.
#'
#' Specific behavior can be tuned with the \code{mode} parameter the values
#' of which range from \code{0L} to \code{2L}, with a lower value
#' corresponding to more relaxed comparison level.
#'
#' \itemize{
#'   \item 0: integer like reals (e.g. \code{1.0}) can match against integer
#'     templates, and integers always match real templates; all
#'     function types are considered of the same type
#'   \item 1: integers always match against numeric templates, but not vice
#'     versa, and integer-like reals are treated only as reals; functions only
#'     match same function type (i.e. closures only match closures, builtins
#'     builtins, and specials specials)
#'   \item 2: types must be equal for all objects types (for functions, this is
#'     unchanged from 1)
#' }
#' DEVNOTE RATIONALIZE THE BELOW WITH vetr_settings
#'
#' mode integer(1L) in 0:2, see details
#'
#' @param fuzzy.int.max.len max length of numeric vectors to consider for
#'   integer likeness (e.g. \code{c(1, 2)} can be considered "integer", even
#'   though it is numeric); currently we limit this check to vectors
#'   shorter than 100 to avoid a potentially expensive computation on large
#'   vectors
#'
#' @seealso type_of, alike, [vetr_settings()]
#' @aliases .type_alike
#' @param target the object to test type alikeness against
#' @param current the object to test the type alikeness of
#' @param settings see 
#' @export

type_alike <- function(target, current, settings=NULL)
  .Call(VALC_type_alike, target, current, settings)
