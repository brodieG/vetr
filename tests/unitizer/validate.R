library(validate)

unitizer_sect("Single template validation", {
  fun0 <- function(x, y, z)
    validate(x=matrix(integer(), ncol=3), y=integer(2L), z=logical(1L))
  fun0(1, 2, 3)
  fun0(matrix(1), 2, 3)
  fun0(matrix(1:3, nrow=1), 2, 3)
  fun0(matrix(1:3, nrow=1), 2:3, 3)
  fun0(matrix(1:3, nrow=1), c(2.0, 3.0), 3)      # integer like
  fun0(matrix(1:3, nrow=1), c(2.0, 3.0), TRUE)
})
unitizer_sect("Multi-template validation", {
  fun1 <- function(x, y, z)
    validate(
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



