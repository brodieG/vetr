# Runs the tests without attempting to fully compose the result message

library(validate)

unitizer_sect("evaluate", {
  validate:::eval_check(quote(logical(2L)), quote(xyz), 1:2)
  validate:::eval_check(quote(logical(2L)), quote(xyz), c(TRUE, FALSE))
  validate:::eval_check(quote(logical(2L)), quote(xyz), c(TRUE, FALSE, TRUE))

  validate:::eval_check(quote(logical(2L) || NULL), quote(xyz), 1:2)
  validate:::eval_check(quote(logical(2L) || NULL), quote(xyz), NULL)
  validate:::eval_check(quote(logical(2L) || NULL), quote(xyz), c(TRUE, TRUE))

  validate:::eval_check(quote(matrix(integer(), nrow=3) || NULL), quote(xyz), matrix(1:21, ncol=7))
  validate:::eval_check(quote(matrix(integer(), nrow=3) || NULL), quote(xyz), 1:21)
  validate:::eval_check(quote(matrix(integer(), nrow=3) || vector("list", 2L)), quote(xyz), list("hello"))
  validate:::eval_check(quote(matrix(integer(), nrow=3) || vector("list", 2L)), quote(xyz), list("hello", "goodbye"))
  validate:::eval_check(quote(matrix(integer(), nrow=3) || list(character(1L), 1L)), quote(xyz), list("hello", "goodbye"))
})
unitizer_sect("evaluate with sub", {
  xyz <- c(TRUE, TRUE)
  validate:::eval_check(quote(logical(2L) && .(all(xyz))), quote(xyz), xyz)
  validate:::eval_check(quote(logical(2L) && .(all(.))), quote(xyz), xyz)
  validate:::eval_check(quote(logical(2L) && .(!any(is.na(.)))), quote(xyz), xyz)
  xyz <- c(TRUE, NA)
  validate:::eval_check(quote(logical(2L) && .(!any(is.na(.)))), quote(xyz), xyz)
  xyz <- c(TRUE, FALSE, TRUE)
  validate:::eval_check(quote(logical(2L) && .(!any(is.na(.)))), quote(xyz), xyz)

  abc1 <- letters[1:5]
  validate:::eval_check(quote(character(5L) && .(all(. %in% letters[1:3]))), quote(abc1), abc1)
  abc2 <- rep("a", 5)
  validate:::eval_check(quote(character(5L) && .(all(. %in% letters[1:3]))), quote(abc2), abc2)

  mat1 <- matrix(1:30, ncol=3)
  validate:::eval_check(
    quote(
      (matrix(numeric(), ncol=3) || matrix(integer(), nrow=10) || character(10L)) &&
      .(length(.) < 100)
    ),
    quote(mat1), mat1
  )
  mat2 <- matrix(1:120, ncol=3)
  validate:::eval_check(
    quote(
      (matrix(numeric(), ncol=3) || matrix(integer(), nrow=10) || character(10L)) &&
      .(length(.) < 100)
    ),
    quote(mat2), mat2
  )
  mat3 <- LETTERS[1:9]
  validate:::eval_check(   # Fail all
    quote(
      (matrix(numeric(), ncol=3) || matrix(integer(), nrow=10) || character(10L)) &&
      .(length(.) < 100)
    ),
    quote(mat3), mat3
  )
  validate:::eval_check(   # Fail all
    quote(
      matrix(numeric(), ncol=3) || matrix(integer(), nrow=10) ||
      character(10L) || .(length(.) > 20)
    ),
    quote(mat3), mat3
  )
})
unitizer_sect("custom expressions", {
  validate:::eval_check(quote(. > 0), quote(x), -1:1)
  validate:::eval_check(quote(. > 0), quote(x), 1)
  validate:::eval_check(quote(. > 0), quote(x), -1)
  validate:::eval_check(quote(. > 0), quote(x), 1:3)
  validate:::eval_check(quote(. > 0), quote(x), NA_integer_)
  validate:::eval_check(quote(. > 0), quote(x), integer())
})
