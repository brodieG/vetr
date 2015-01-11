library(validate)

unitizer_sect("Tokens", {

} )


unitizer_sect("Single template validation", {
  fun0 <- function(x, y, z)
    validate_args(x=matrix(integer(), ncol=3), y=integer(2L), z=logical(1L))
  fun0(1, 2, 3)
  fun0(matrix(1), 2, 3)
  fun0(matrix(1:3, nrow=1), 2, 3)
  fun0(matrix(1:3, nrow=1), 2:3, 3)
  fun0(matrix(1:3, nrow=1), c(2.0, 3.0), 3)      # integer like
  fun0(matrix(1:3, nrow=1), c(2.0, 3.0), TRUE)
})
unitizer_sect("Multi-template validation", {
  fun1 <- function(x, y, z)
    validate_args(
      x=matrix(integer(), ncol=3) || integer(3L),
      y=integer(2L) || NULL || logical(1L),
      z=logical(1L)
    )
  fun1(1:3, "fail", "fail")                   # x passes
  fun1(matrix(1:9, ncol=3), "fail", "fail")   # x passes
  fun1(letters[1:3], "fail", "fail")          # x fails

  fun1(1:3, 1:2, "fail")                      # x,y pass
  fun1(1:3, NULL, "fail")                     # x,y pass
  fun1(1:3, FALSE, "fail")                    # x,y pass

  fun1(1:3, FALSE, FALSE)                     # all pass
})
unitizer_sect("Template and Straight Eval", {
  fun2 <- function(x, y, z)
    validate_args(
      x=(matrix(integer(), ncol=3) || integer(3L)) && .(!any(is.na(.))),
      y=integer(3L) && .(all(. > 0)),
      z=logical(1L) && .(!is.na(.))
    )
  fun2(matrix(c(1:8, NA), nrow=3), NULL, NULL)
  fun2(matrix(c(1:9), nrow=3), -1:1, NULL)
  fun2(matrix(c(1:9), nrow=3), 1:3, NA)
  fun2(matrix(c(1:9), nrow=3), 1:3, TRUE)
})
unitizer_sect("Errors in Arguments", {
  fun3 <- function(x, y)
    validate_args(x=logical(1L), y=integer(3L))
  fun3(stop("boom"))
  fun3(TRUE, stop("boomBOOM"))
  fun3(1:3, stop("boomBOOM"))

  fun4 <- function(x, y)
    validate_args(x=stop("BOOM"), y=integer(3L))
  fun4(NULL, 1:3)

  fun5 <- function(x, y)
    validate_args(x=integer(3L), y=NULL || .(stop("hah")))
  fun5(1:3, NULL)
  fun5(1:2, NULL)

  fun6 <- function(x, y)
    validate_args(x=integer(3L), y=NULL && .(stop("hah")))
  fun6(1:3, NULL)
})
unitizer_sect("Args evaled in correct env?", {
  fun7 <- function(x, y=z + 2) { z <- "boom"; validate_args(x=TRUE, y=1L) }
  fun7a <- function(x, y=z + 2) { z <- 40; validate_args(x=TRUE, y=1L) }
  z <- 1
  fun7(TRUE)     # fail because z in fun is character
  fun7a(TRUE)    # works
  fun8 <- function(x, y=z + 2) { a <- b <- TRUE; validate_args(x=TRUE, y=1L) }
  fun8a <- function(x, y=z + 2) { a <- b <- NULL; validate_args(x=TRUE, y=1L) }
  a <- NULL
  b <- TRUE
  fun8(a && b)   # fail because a in parent is list
  a <- TRUE
  fun8a(a && b)  # works despite NULLs in function
})
