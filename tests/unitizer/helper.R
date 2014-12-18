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
  validate:::eval_check(quote(matrix(integer(), nrow=3) || vector("list", 2L)), quote(xyz), list(NULL, NULL))

  xyz <- c(TRUE, TRUE)
  validate:::eval_check(quote(logical(2L) && .(!any(is.na(.)))), quote(xyz), xyz)
  xyz <- c(TRUE, NA)
  validate:::eval_check(quote(logical(2L) && .(!any(is.na(.)))), quote(xyz), xyz)
  xyz <- c(TRUE, FALSE, TRUE)
  validate:::eval_check(quote(logical(2L) && .(!any(is.na(.)))), quote(xyz), xyz)
})
