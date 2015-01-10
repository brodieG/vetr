library(validate)

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
unitizer_sect("preset tokens", {
  x <- quote(integer(1L))
  y <- quote(integer(1L) || NULL)
  z <- quote(integer(1L) && .(!any(is.na(.))))
  validate:::parse_validator(quote(x), quote(w))
  validate:::parse_validator(quote(y), quote(w))
  validate:::parse_validator(quote(z), quote(w))
  validate:::parse_validator(quote(z || NULL), quote(w))
} )
unitizer_sect("validators", {
  validate:::parse_validator(INT1, quote(w))
  validate:::parse_validator(INT, quote(w))
  validate:::parse_validator(CHR1, quote(w))
  validate:::parse_validator(CHR, quote(w))
  validate:::parse_validator(NUM1, quote(w))
  validate:::parse_validator(NUM, quote(w))
  validate:::parse_validator(LGL1, quote(w))
  validate:::parse_validator(LGL, quote(w))
  validate:::parse_validator(CPX1, quote(w))
  validate:::parse_validator(CPX, quote(w))
} )
