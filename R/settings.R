# Initial list of settings to track
#
# alike settings
# VALC_max_chars
# CSR_max_chars (reconcile with above)
# Recursion depth for environments
# Recursion depth for vetting expression substitution
# track_hash contents initial size (64)
# VALC_name_sub max symbol 15K
#
# Get rid of the frame, deprecate `.alike`



#' Generate Control Settings For Vetr and Alike
#'
#' Utility function to generate setting values.  We strongly recommend
#' that you generate the settings outside of function calls so that setting
#' generation does not become part of the \code{vet/vetr/alike} evaluation as
#' that could add noticeable overhead to the function evaluation.
#'
#' Note that a successful evaluation of this function does not guarantee a
#' correct settings list.  Those checks are carried out internally by
#' \code{vet/vetr/alike}.
#'
#' @seealso \code{\link{type_alike}}, \code{\link{alike}}, \code{\link{vetr}}
#' @param type.mode integer(1L) in 0:2 how strict to be about type comparison,
#'   see \code{\link{type_alike}}, with 0, the default, the most flexible.
#' @param type.mode integer(1L) in 0:2, see \code{mode} parameter to
#'   \code{\link{type_alike}}
#' @param attr.mode integer(1L) in 0:2 determines strictness of attribute
#'   comparison: \itemize{
#'     \item \code{0} only checks attributes that are present in target, and
#'       uses special comparisons for the special attributes (\code{class},
#'       \code{dim}, \code{dimnames}, \code{names}, \code{row.names},
#'       \code{levels}, \code{srcref}, and \code{tsp}) while requiring other
#'       attributes to be \code{alike}
#'     \item \code{1} is like \code{0}, except all atributes must be
#'       \code{alike}
#'     \item \code{2} requires all attributes to be present in \code{target} and
#'       \code{current} and to be alike
#'   }
#' @param lang.mode integer(1L) in 0:1 controls language matching, set to `1` to
#'   turn off use of \code{\link{match.call}}
#' @param fun.mode NOT IMPLEMENTED, controls how functions are compared
#' @param rec.mode integer(1L) `0` currently unused, intended to control how
#'   recursive structures (other than language objects) are compared
#' @param fuzzy.int.max.len see same parameter for \code{\link{type_alike}}
#' @param suppress.warnings logical(1L)
#' @param width to use when deparsing expressions; default `-1`
#'   equivalent to \code{getOption("width")}
#' @param env.depth.max integer(1L) maximum number of nested environments to
#'   recurse through; these are tracked to make sure we do not get into an
#'   infinite recursion loop, but because they are tracked we keep a limit on how
#'   many we will go through.
#' @param symb.sub.depth.max integer(1L) maximum recursion depth when
#'   recursively substituting symbols in vetting expression

vetr_settings <- function(
  type.mode=0L, attr.mode=0L, lang.mode=0L, fun.mode=0L, rec.mode=0L,
  suppress.warnings=FALSE, fuzzy.int.max.len=100L,
  width=-1L, env.depth.max=65535L, symb.sub.depth.max=65535L,
  nchar.max=65535L
) {
  as.list(environment)
}