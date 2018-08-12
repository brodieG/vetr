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

#' Turn S3 Objects Into Templates
#'
#' Create templates for use by \code{\link{alike}}. Currently somewhat
#' experimental; behavior may change in future.
#'
#' \code{abstract} is intended to create templates for use by
#' \code{\link{alike}}.  The result of abstraction is often a partially
#' specified object.  This type of object may not be suited for use in typical
#' R computations and may cause errors (or worse) if you try to use them as
#' normal R objects.
#'
#' There is no guarantee that the \code{abstract}ed object is suitable for use
#' as a template to \code{alike} as is.  You may need to modify it further so
#' that it suits your purposes.
#'
#' \code{abstract} is an S3 generic.  The default method will
#' dispatch on implicit classes, so if you attempt to \code{abstract} an object
#' without an explicit \code{abstract} method, it will get abstracted based on
#' its implicit class.  If you define your own \code{abstract} method and do not
#' wish further abstraction based on implicit classes do not use
#' \code{\link{NextMethod}}.
#'
#' S4 and RC objects are returned unchanged.
#'
#' @section Time Series:
#'
#' \code{\link{alike}} will treat time series parameter components with zero in
#' them as wildcards.  This function allows you to create these wild card time
#' series attributes since R does not allow direct creation/modification of
#' \code{ts} attributes with zero values.
#'
#' Make sure you do not try to use the templates you create with this for
#' anything other than as \code{\link{alike}} templates since the result is
#' likely undefined given R expects non zero values for the \code{ts}
#' attribute and attempts to prevent such attributes.
#'
#' @export
#' @param x the object to abstract
#' @param ... arguments for methods that require further arguments
#' @param what, for time series which portion of the \code{ts} attribute to
#'   abstract, by default all three are abstracted, but you can select, any one,
#'   two, or all
#' @return abstracted object
#' @examples
#' iris.tpl <- abstract(iris)
#' alike(iris.tpl, iris[1:10, ])
#' alike(iris.tpl, transform(iris, Species=as.character(Species)))
#'
#' abstract(1:10)
#' abstract(matrix(1:9, nrow=3))
#' abstract(list(1:9, runif(10)))

abstract <- function(x, ...) UseMethod("abstract")

#' @rdname abstract
#' @export

abstract.data.frame <- function(x, ...) x[0, ]

#' @importFrom utils modifyList
#' @rdname abstract
#' @export

abstract.default <- function(x, ...) {
  if(isS4(x)) return(x);
  if(!is.null(class.exp <- attr(x, "class"))) {
    attr(x, "class") <- NULL
    x <- abstract(x, ...)  # handle implicit classes
    class(x) <- class.exp
  }
  if(!is.atomic(x)) return(x)
  attrs.old <- attributes(x)
  length(x) <- 0L
  attrs.new <- attributes(x)
  attributes(x) <- if(!is.null(attrs.new)) modifyList(attrs.old, attrs.new)
  else attrs.old
  x
}
#' @rdname abstract
#' @export

abstract.array <- function(x, ...) {
  if(!is.atomic(x)) NextMethod()
  ndims <- length(dim(x))
  length(x) <- 0L
  dim(x) <- rep(0L, ndims)
  # nocov start previous line should get rid of dimnames already so
  # there is no way to test this, but leaving it here just in case
  if(!is.null(dimnames(x)))
    dimnames(x) <- replicate(NULL, length(dimnames(x)), simplify=FALSE)
  # nocov end
  x
}
#' @rdname abstract
#' @export

abstract.matrix <-function(x, ...) abstract.array(x, ...)

#' @rdname abstract
#' @export

abstract.list <- function(x, ...) {
  for(i in seq_along(x)) {
    xi.abs <- abstract(x[[i]])
    if(is.null(xi.abs)) x <- nullify(x, i)
    else x[[i]] <- xi.abs
  }
  x
}
#' @rdname abstract
#' @export

abstract.lm <- function(x, ...) {
  names(attr(x$terms, "dataClasses")) <- NULL
  names(x$model) <- NULL
  names(attr(attr(x$model, "terms"), "dataClasses")) <- NULL
  attr(attr(x$model, "terms"), ".Environment") <- emptyenv()
  attr(x$terms, ".Environment") <- emptyenv()
  # zero length call should match any call
  x$call <- call(as.character(x$call[[1L]]))
  NextMethod()
}
# nocov start
#' Experimental Abstraction Method for GGPlot
#'
#' Not entirely sure this can ever work well since so much of \code{ggplot} is
#' done with \code{proto} objects and those do not really use meta data, which
#' makes \code{alike} rather useless.
#'
#' @keywords internal
#' @export

abstract.ggplot <- function(x, ...) {
  x <- NextMethod()
  x$data <- data.frame()
  x
}
# nocov end
#' @rdname abstract
#' @export

abstract.environment <- function(x, ...) x

#' @rdname abstract
#' @export

abstract.ts <- function(x, what=c("start", "end", "frequency"), ...) {
  what.valid <- c("start", "end", "frequency")
  if(
    !is.character(what) || any(is.na(what)) || !all(what %in% what.valid)
  )
    stop(
      "Argument `what` must be character with all values in ",
      deparse(what.valid)
    )
  tsp <- attr(x, "tsp")
  if(!is.numeric(tsp) || length(tsp) != 3L)
    stop("Argument `x` must have a \"tsp\" attribute that is numeric(3L)")
  attr(x, "tsp") <- NULL
  x <- abstract.default(x, ...)

  tsp[match(unique(what), what.valid)] <- 0
  .Call(VALC_abstract_ts, x, tsp)
}
#' Set Element to NULL Without Removing It
#'
#' This function is required because there is no straightforward way to
#' over-write a value in a list with NULL without completely removing the entry
#' from the list as well.
#'
#' This returns a copy of the object modified with null slots; it does
#' not modify the input argument.
#'
#' Default method will attempt to convert non-list objects to lists
#' with \code{\link{as.list}}, and then back to whatever they were by using a
#' function with name \code{paste0("as.", class(obj)[[1L]])}
#' if it exists and works.  If the object cannot be coerced back
#' to its original type the corresponding list will be returned.
#'
#' If this is not appropriate for your object type you can write an S3 method
#' for it.
#'
#' @note attributes are copied from original object and re-applied to final
#'   object before return, which may
#'   not make sense in some circumstances.
#'
#' @export
#' @param obj the R object to NULL a value in
#' @param index an indexing vectors of values to NULL
#' @return object with selected values NULLified
#' @examples
#' nullify(list(1, 2, 3), 2)
#' nullify(call("fun", 1, 2, 3), 2)

nullify <- function(obj, index) {
  UseMethod("nullify")
}
#' @rdname nullify
#' @export

nullify.default <- function (obj, index) {
  not.list <- FALSE
  if(!is.list(obj)) {
    not.list <- TRUE
    class <- class(obj)
    if(inherits(try(obj <- as.list(obj)), "try-error"))
      stop("Could not coerce `obj` to list; see previous error.")
  }
  if(
    !is.character(index) &&
    !(is.numeric(index) && all(index >= 1L)) && !is.logical(index)
  ) {
    stop(
      "Argument `index` must be a valid subsetting index and if numeric must ",
      "be greater than or equal to one."
    )
  }
  seq.along <- seq_along(obj)
  if(is.numeric(index)) {
    if(min(index) < min(seq.along) || max(index) > max(seq.along)) {
      stop(
        "Argument `index` can only contain values that exist within ",
        "seq_along(`obj`)."
      )
    }
    vec.subset <- seq.along %in% index
  } else if(is.logical(index)) {
    if(length(index) > length(obj)) {
      stop("Argument `index` may be no longer than `obj` if it is logical.")
    }
    else if(length(obj) %% length(index) > 0L) {
      warning("Argument `obj` is not a multiple in length of argument `obj`")
    }
    vec.subset <- rep(index, ceiling(length(obj) / length(index)))[seq.along]
  } else if (is.character(index)) {
    ind.match <- index %in% names(obj)
    if(!all(ind.match)) {
      stop(
        "Argument `index` contains values not present in names of `obj` (",
        paste(index[!ind.match], collapse=", "), ")."
      )
    }
    vec.subset <- names(obj) %in% index
  }
  res <- ifelse(vec.subset, lapply(seq.along, function(x) NULL), obj)
  if(not.list) {  # try to reconvert object
    if(
      inherits(
        try(
          res.conv <- eval(call(paste0("as.", class[[1L]]), res)), silent=TRUE
        ),
        "try-error"
      )
    ) {
      warning("Unable to convert object back to class \"", class[[1L]],"\"")
    } else {
      res <- res.conv
    }
  }
  attrs.new <- attributes(res)
  attributes(res) <- if(!is.null(attrs.new))
    modifyList(attributes(obj), attrs.new) else attributes(obj)
  res
}

