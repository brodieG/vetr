library(vetr)

unitizer_sect("name_sub", {
  vetr:::name_sub(quote(.), quote(xyz))
  vetr:::name_sub(quote(.), quote(x + yz))   # Works, but may break parent fun
  vetr:::name_sub(quote(.), c(1:3))          # Works, but may break parent fun
  vetr:::name_sub(quote(..), quote(xyz))
  vetr:::name_sub(quote(...), quote(xyz))
  vetr:::name_sub(quote(.zzz), quote(xyz))
  vetr:::name_sub(quote(zzz.), quote(xyz))
  vetr:::name_sub(quote(zzz), quote(xyz))
  vetr:::name_sub(quote(a + b), quote(xyz))  # Does nothing
  vetr:::name_sub(quote(. + .), quote(xyz))  # Does nothing
  vetr:::name_sub(quote(.(zzz)), quote(xyz)) # Does nothing
  vetr:::name_sub("hello", quote(xyz))       # Does nothing
})
unitizer_sect("remove parens", {
  vetr:::remove_parens(quote((a)))
  vetr:::remove_parens(quote(.(a)))
  vetr:::remove_parens(quote((((a)))))
  vetr:::remove_parens(quote((.((.(a))))))
  vetr:::remove_parens(quote((a) && .(a)))  # Nothing should be removed
})
unitizer_sect("parse", {
  x <- quote(.(.) && ((a)))
  vetr:::parse_validator(x, quote(arg_to_validate))
  x # make sure unchanged from previous assignment

  vetr:::parse_validator(quote(FALSE), quote(arg_to_validate))
  vetr:::parse_validator(quote(((FALSE))), quote(arg_to_validate))
  vetr:::parse_validator(quote(((FALSE && ((TRUE))))), quote(arg_to_validate))
  vetr:::parse_validator(quote(.(FALSE)), quote(arg_to_validate))
  vetr:::parse_validator(quote(.), quote(arg_to_validate))
  vetr:::parse_validator(quote(. && a), quote(arg_to_validate))
  vetr:::parse_validator(quote(.(.)), quote(arg_to_validate))
  vetr:::parse_validator(quote(((a && b) || .(.))), quote(arg_to_validate))
  vetr:::parse_validator(quote(matrix(nrow=3)), quote(arg_to_validate))
  vetr:::parse_validator(quote(matrix(nrow=3) && .(.)), quote(arg_to_validate))
  vetr:::parse_validator(quote((a || ((b && c))) && .(a + .)), quote(arg_to_validate))
  vetr:::parse_validator(quote((a || ((b && .(c)))) && (a + .(.))), quote(arg_to_validate))

  vetr:::parse_validator(quote(a && (b + .(c))), quote(arg_to_validate))  # uninterpretable?
  vetr:::parse_validator(quote(a && .), "hello")                          # uninterpretable?
} )
unitizer_sect("token sub", {
  vetr:::symb_sub(INT.1)
  vetr:::symb_sub(NO.NA)
})

unitizer_sect("preset tokens", {
  x <- quote(integer(1L))
  y <- quote(integer(1L) || NULL)
  z <- quote(integer(1L) && .(!any(is.na(.))))
  vetr:::parse_validator(quote(x), quote(w))
  vetr:::parse_validator(quote(y), quote(w))
  vetr:::parse_validator(quote(z), quote(w))
  vetr:::parse_validator(quote(z || NULL), quote(w))
} )
unitizer_sect("validators", {
  vetr:::parse_validator(INT.1, quote(w))
  vetr:::parse_validator(INT, quote(w))
  vetr:::parse_validator(CHR.1, quote(w))
  vetr:::parse_validator(CHR, quote(w))
  vetr:::parse_validator(NUM.1, quote(w))
  vetr:::parse_validator(NUM, quote(w))
  vetr:::parse_validator(LGL.1, quote(w))
  vetr:::parse_validator(LGL, quote(w))
  vetr:::parse_validator(CPX.1, quote(w))
  vetr:::parse_validator(CPX, quote(w))
} )
