library(vetr)

unitizer_sect("Single template validation", {
  fun0 <- function(x, y, z)
    vetr(x=matrix(integer(), ncol=3), y=integer(2L), z=logical(1L))
  fun0(1, 2, 3)
  fun0(matrix(1), 2, 3)
  fun0(matrix(1:3, nrow=1), 2, 3)
  fun0(matrix(1:3, nrow=1), 2:3, 3)
  fun0(matrix(1:3, nrow=1), c(2.0, 3.0), 3)      # integer like
  fun0(matrix(1:3, nrow=1), c(2.0, 3.0), TRUE)
})
unitizer_sect("Multi-template validation", {
  fun1 <- function(x, y, z)
    vetr(
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
    vetr(
      x=(matrix(integer(), ncol=3) || integer(3L)) && .(!any(is.na(.))),
      y=integer(3L) && .(all(. > 0)),
      z=logical(1L) && .(!is.na(.))
    )
  fun2(matrix(c(1:8, NA), nrow=3), NULL, NULL)
  fun2(matrix(c(1:9), nrow=3), -1:1, NULL)
  fun2(matrix(c(1:9), nrow=3), 1:3, NA)
  fun2(matrix(c(1:9), nrow=3), 1:3, TRUE)
})
unitizer_sect("Complex OR outcomes", {
  fun2a <- function(x)
    vetr(
      x=setNames(character(3L), letters[1:3]) || matrix("", 3, 1) ||
        list(character(), x=integer())
    )
  fun2a(letters[1:3])
})
unitizer_sect("Errors in Arguments", {
  fun3 <- function(x, y)
    vetr(x=logical(1L), y=integer(3L))
  fun3(stop("boom"))
  fun3(TRUE, stop("boomBOOM"))
  fun3(1:3, stop("boomBOOM"))

  fun4 <- function(x, y)
    vetr(x=stop("BOOM"), y=integer(3L))
  fun4(NULL, 1:3)

  fun5 <- function(x, y)
    vetr(x=integer(3L), y=NULL || .(stop("hah")))
  fun5(1:3, NULL)
  fun5(1:2, NULL)

  fun6 <- function(x, y)
    vetr(x=integer(3L), y=NULL && .(stop("hah")))
  fun6(1:3, NULL)
})
unitizer_sect("Args evaled in correct env?", {
  fun7 <- function(x, y=z + 2) { z <- "boom"; vetr(x=TRUE, y=1L) }
  fun7a <- function(x, y=z + 2) { z <- 40; vetr(x=TRUE, y=1L) }
  z <- 1
  fun7(TRUE)     # fail because z in fun is character
  fun7a(TRUE)    # works
  fun8 <- function(x, y=z + 2) { a <- b <- TRUE; vetr(x=TRUE, y=1L) }
  fun8a <- function(x, y=z + 2) { a <- b <- NULL; vetr(x=TRUE, y=1L) }
  a <- NULL
  b <- TRUE
  fun8(a && b)   # fail because a in parent is NULL
  a <- TRUE
  fun8a(a && b)  # works despite NULLs in function

  # Make sure we can access defined templates in lexical parents

  fun_make <- function() {
    a <- matrix(1:9, 3)
    tpl <- matrix(numeric(), 3)

    function(x) {
      vetr(tpl)
      TRUE
    }
  }
  fun <- fun_make()
  a <- b <- 1:9
  local({
    NULL
    a <- character()
    fun(a)
  })
  local({
    b <- character()
    fun(b)
  })
  # make sure we can access variables that are not in fun lexical scope

  fun8b <- function(x) vetr(x=length(.) > 0 && integer())
  get("zfqwefkj")  # should fail
  local({
    zfqwefkj <- 200L
    fun8b(zfqwefkj)
  })
})
unitizer_sect("Compound Expression Scope Issues", {
  a <- quote(!anyNA(.))
  fun <- function(x) {
    a <- quote(all(. > 0))
    b <- quote(is.vector(.))
    vetr(a && b)
    TRUE
  }
  fun(-(1:3))
})
unitizer_sect("Non-equal args and validation exps", {
  fun8 <- function(x="hello", y=TRUE, z)
    vetr(x=integer(), z=integer(2L))

  fun8(1L, NULL, 1:2)
  fun8(1L, 1:2, NULL)

  fun8(1L, 1:2)
  fun8(1L)

  # default argument fails validation

  fun8(z=1:2)

})
unitizer_sect("Referencing argument in vet exp error", {
  fun1 <- function(x, y) vetr(x > 0, . < 3)
  fun1(1:10, 1:10)
  fun2 <- function(x, y) vetr(. > 0 && all(y > 0), y < 3)
  fun2(TRUE, 1:10)

  # also check with vet, although not as important

  x <- 1:10
  vet(x > 0, x)
  vet((x + 1) > 0, x + 1)  # this doesn't cause error, but maybe should?
})
unitizer_sect("Default arg mix-up", {
  fun10a <- function(x, y=TRUE, z=999) vetr(INT, LGL.1, INT.1)
  fun10a(1, z=1:3)

  fun10b <- function(x, y=TRUE, z=999) vetr(INT, z=INT.1)
  fun10b(1, z=1:3)
})
