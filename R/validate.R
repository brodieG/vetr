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

validate <- function(...) {
  if(identical(parent.frame(1L), sys.frame(0L))) {
    stop("Parent frame is R_GlobalEnv, so it seems you are not running this function from a closure")
  }
  par.frame <- parent.frame()
  par.call <- match_call(dots="include", n=2L, default.formals=TRUE, empty.formals=TRUE)
  frms <- as.list(par.call)[-1L]    # The formals to the function that uses `check_args`
  args <- as.list(sys.call())[-1L]  # The arguments to `check_args`

  named.match <- intersect(names(frms), names(args))
  if(is.null(named.match)) named.match <- character(0L)
  frms.matched <- frms[names(frms) %in% named.match]
  frms.remaining <- frms[!(names(frms) %in% named.match)]
  arg.names <- names(args)
  if(is.null(arg.names)) arg.names <- character(length(args))
  args.matched <- args[arg.names %in% named.match]
  args.remaining <- args[!(arg.names %in% named.match)]

  if(length(args.remaining) > length(frms.remaining)) {
    stop("Cannot check arguments ", length(frms.remaining) + 1L, " and greater ",
      "since `", par.call[[1]], "` only has ", length(frms.remaining), " arguments.")
  } else if (length(args.remaining) > 0L) {
    frms.final <- c(frms.matched, frms.remaining[1L:length(args.remaining)])
  } else {
    frms.final <-frms.matched
  }
  args.final <- c(args.matched, args.remaining)

  rm(frms, args, frms.matched, frms.remaining, args.matched, args.remaining)

  # Eval formals and the check args values; for the former eval the names in
  # parent environment so that user set variables are evaluated in the grandparent
  # and default ones in the parent.

  if(inherits(
    try(frms.final.ev <- lapply(lapply(names(frms.final), as.name), eval, parent.frame())),
    "try-error"
  ) ) {
    stop("Failed attempting to evaluate `", par.call[[1]], "` formals.")
  }
  # If check is not of c_group type, make it so to simplify rest of process; note
  # this is computationally expensive so we should reconsider

  args.final.ev <- tryCatch(
    lapply(
      args.final,
      function(arg.final) {
        arg.final.ev <- eval(arg.final, par.frame)
        if(!inherits(arg.final.ev, "c_group")) {
          new.call <- as.call(list(quote(c_and), arg.final))
          arg.final.ev <- eval(new.call, par.frame)
        }
        arg.final.ev
    } ),
    error=function(e) stop("Improper Arguments; see previous error.")
  )
  # Compare the formals to the check args values

  frms.raw <- formals(as.character(par.call[[1L]]))

  for(i in seq_along(args.final.ev)) {
    res <- process(args.final.ev[[i]], frms.final.ev[[i]], args.final[[i]], names(frms.final)[[i]])
    errors <- unlist(Filter(is.character, res))
    if(length(errors) > 0L) {
      if(length(errors) > 1L) {
        err.opts <- paste(paste0(errors[-length(errors)], collapse=", "), errors[length(errors)], sep=", and ")
      } else {
        err.opts <- errors
      }
      msg <- paste0("Problem with argument `", names(frms.final)[[i]], "`: ", err.opts)

      stop(simpleError(msg, par.call))
} } }
