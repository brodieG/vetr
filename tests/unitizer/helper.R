unitizer_sect("name_sub", {
  validate:::name_sub(quote(.), quote(xyz))
  validate:::name_sub(quote(.), quote(x + yz))   # Works, but may break parent fun
  validate:::name_sub(quote(.), c(1:3))          # Works, but may break parent fun
  validate:::name_sub(quote(..), quote(xyz))
  validate:::name_sub(quote(...), quote(xyz))
  validate:::name_sub(quote(.zzz), quote(xyz))
  validate:::name_sub(quote(zzz.), quote(xyz))
  validate:::name_sub(quote(zzz), quote(xyz))
  validate:::name_sub(quote(a + b), quote(xyz))  # Does nothing
  validate:::name_sub(quote(. + .), quote(xyz))  # Does nothing
  validate:::name_sub(quote(.(zzz)), quote(xyz)) # Does nothing
  validate:::name_sub("hello", quote(xyz))       # Does nothing
})
unitizer_sect("remove parens", {
  validate:::remove_parens(quote((a)))
  validate:::remove_parens(quote(.(a)))
  validate:::remove_parens(quote((((a)))))
  validate:::remove_parens(quote((.((.(a))))))
  validate:::remove_parens(quote((a) && .(a)))  # Nothing should be removed
})
unitizer_sect("parse", {
  x <- quote(.(.) && ((a)))
  validate:::parse_validator(x, quote(arg_to_validate))
  x # make sure unchanged from previous assignment

  validate:::parse_validator(quote(FALSE), quote(arg_to_validate))
  validate:::parse_validator(quote(((FALSE))), quote(arg_to_validate))
  validate:::parse_validator(quote(((FALSE && ((TRUE))))), quote(arg_to_validate))
  validate:::parse_validator(quote(.(FALSE)), quote(arg_to_validate))
  validate:::parse_validator(quote(.), quote(arg_to_validate))
  validate:::parse_validator(quote(. && a), quote(arg_to_validate))
  validate:::parse_validator(quote(.(.)), quote(arg_to_validate))
  validate:::parse_validator(quote(((a && b) || .(.))), quote(arg_to_validate))
  validate:::parse_validator(quote(matrix(nrow=3)), quote(arg_to_validate))
  validate:::parse_validator(quote(matrix(nrow=3) && .(.)), quote(arg_to_validate))
  validate:::parse_validator(quote((a || ((b && c))) && .(a + .)), quote(arg_to_validate))
  validate:::parse_validator(quote((a || ((b && .(c)))) && (a + .(.))), quote(arg_to_validate))

  validate:::parse_validator(quote(a && (b + .(c))), quote(arg_to_validate))  # uninterpretable?
  validate:::parse_validator(quote(a && .), "hello")                          # uninterpretable?
} )
unitizer_sect("parse with preset tokens", {
  x <- quote(integer(1L))
  y <- quote(integer(1L) || NULL)
  z <- quote(integer(1L) && .(!any(is.na(.))))
  validate:::parse_validator(quote(x), quote(w))
  validate:::parse_validator(quote(y), quote(w))
  validate:::parse_validator(quote(z), quote(w))
  validate:::parse_validator(quote(z || NULL), quote(w))
} )
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
unitizer_sect("All", {
  validate:::val_all(1:10)  # -2
  validate:::val_all(rep(TRUE, 10)) # 1
  validate:::val_all(c(rep(TRUE, 10), FALSE, TRUE)) # 0
  validate:::val_all(c(rep(TRUE, 5), NA, rep(TRUE, 5))) # 0
  validate:::val_all(FALSE) # -1
})
