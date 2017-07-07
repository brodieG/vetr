# Runs the tests without attempting to fully compose the result message

library(vetr)

unitizer_sect("evaluate", {
  vetr:::eval_check(quote(logical(2L)), quote(xyz), 1:2)
  vetr:::eval_check(quote(logical(2L)), quote(xyz), c(TRUE, FALSE))
  vetr:::eval_check(quote(logical(2L)), quote(xyz), c(TRUE, FALSE, TRUE))

  vetr:::eval_check(quote(logical(2L) || NULL), quote(xyz), 1:2)
  vetr:::eval_check(quote(logical(2L) || NULL), quote(xyz), NULL)
  vetr:::eval_check(quote(logical(2L) || NULL), quote(xyz), c(TRUE, TRUE))

  vetr:::eval_check(
    quote(matrix(integer(), nrow=3) || NULL), quote(xyz), matrix(1:21, ncol=7)
  )
  vetr:::eval_check(quote(matrix(integer(), nrow=3) || NULL), quote(xyz), 1:21)
  vetr:::eval_check(
    quote(matrix(integer(), nrow=3) || vector("list", 2L)),
    quote(xyz), list("hello")
  )
  vetr:::eval_check(
    quote(matrix(integer(), nrow=3) || vector("list", 2L)),
    quote(xyz), list("hello", "goodbye")
  )
  vetr:::eval_check(
    quote(matrix(integer(), nrow=3) || list(character(1L), 1L)),
    quote(xyz), list("hello", "goodbye"))
})
unitizer_sect("evaluate with sub", {
  xyz <- c(TRUE, TRUE)
  vetr:::eval_check(quote(logical(2L) && .(all(xyz))), quote(xyz), xyz)
  vetr:::eval_check(quote(logical(2L) && .(all(.))), quote(xyz), xyz)
  vetr:::eval_check(quote(logical(2L) && .(!any(is.na(.)))), quote(xyz), xyz)
  xyz <- c(TRUE, NA)
  vetr:::eval_check(quote(logical(2L) && .(!any(is.na(.)))), quote(xyz), xyz)
  xyz <- c(TRUE, FALSE, TRUE)
  vetr:::eval_check(quote(logical(2L) && .(!any(is.na(.)))), quote(xyz), xyz)

  abc1 <- letters[1:5]
  vetr:::eval_check(
    quote(character(5L) && .(all(. %in% letters[1:3]))), quote(abc1), abc1
  )
  abc2 <- rep("a", 5)
  vetr:::eval_check(
    quote(character(5L) && .(all(. %in% letters[1:3]))), quote(abc2), abc2
  )

  mat1 <- matrix(1:30, ncol=3)
  vetr:::eval_check(
    quote(
      (
        matrix(numeric(), ncol=3) || matrix(integer(), nrow=10) ||
        character(10L)
      ) && .(length(.) < 100)
    ),
    quote(mat1), mat1
  )
  mat2 <- matrix(1:120, ncol=3)
  vetr:::eval_check(
    quote(
      (
        matrix(numeric(), ncol=3) || matrix(integer(), nrow=10) ||
        character(10L)
      ) &&
      .(length(.) < 100)
    ),
    quote(mat2), mat2
  )
  mat3 <- LETTERS[1:9]
  vetr:::eval_check(   # Fail all
    quote(
      (
        matrix(numeric(), ncol=3) || matrix(integer(), nrow=10) ||
        character(10L)
      ) && .(length(.) < 100)
    ),
    quote(mat3), mat3
  )
  vetr:::eval_check(   # Fail all
    quote(
      matrix(numeric(), ncol=3) || matrix(integer(), nrow=10) ||
      character(10L) || .(length(.) > 20)
    ),
    quote(mat3), mat3
  )
})
unitizer_sect("custom expressions", {
  x <- -1:1
  y <- 1
  z <- -1
  w <- NA_integer_
  u <- integer()
  t <- 1:3
  vetr:::eval_check(quote(. > 0), quote(x), x)
  vetr:::eval_check(quote(. > 0), quote(y), y)
  vetr:::eval_check(quote(. > 0), quote(z), z)
  vetr:::eval_check(quote(. > 0), quote(t), t)
  vetr:::eval_check(quote(. > 0), quote(w), w)
  vetr:::eval_check(quote(. > 0), quote(u), u)
})
unitizer_sect("Errors", {
  vetr:::eval_check(1:3, 1:3, TRUE, env=list(1:3))
  vetr:::eval_check(quote(y), quote(x), TRUE, env=list(1:3))
})
