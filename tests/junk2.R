fun3 <- function(a, b, c=5L) {
  check_args(
    a=numeric(), 
    b=c_and(c_or(is.integer(b), is.character(b)), is.matrix(b)), 
    c=data.frame()
  )
}
check_args2 <- function(...) {
  if(identical(parent.frame(1L), sys.frame(0L))) {
    stop("Parent frame is R_GlobalEnv, so it seems you are not running this function from a closure")
  }
  par.frame <- parent.frame()
  par.call <- match_call(dots="include", parent.offset=1L, default.formals=TRUE, empty.formals=TRUE)
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
  # If check is not of c_group type, make it so to simplify rest of process
  
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

process <- function(...) {
  UseMethod("process")
}
#' @method process default
#' @S3method process default

process.default <- function(check, frm, check.calls, frm.name) {
  if(frm.name %in% all.vars(check.calls, unique=TRUE)) {
    if(!identical(check, TRUE)) {  # Type 2 check
      return(list(paste0(deparse(check.calls), " != TRUE")))
    }
  } else {               # Type 1 check
    struct.check <- alike(check, frm)
    if(!isTRUE(struct.check)) return(list(struct.check))
  } 
  list(TRUE)
}
#' @method process c_and
#' @S3method process c_and

process.c_and <- function(check, frm, check.calls, frm.name) {  
  res <- list()
  for(i in seq_along(check)) {
    res <- c(res, process(check[[i]], frm , attr(check, "check.calls")[[i]], frm.name))
    if(res[[length(res)]] != TRUE) return(res)  # this will either be a single TRUE, or one character string, or many character strings
  }
  return(list(TRUE))
}
#' @method process c_or
#' @S3method process c_or

process.c_or <- function(check, frm, check.calls, frm.name) {
  
  res <- list()
  for(i in seq_along(check)) {
    res <- c(res, process(check[[i]], frm , attr(check, "check.calls")[[i]], frm.name))
    if(res[[length(res)]] == TRUE) return(list(TRUE))
  }
  return(res)
}