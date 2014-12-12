unitizer_sect("name_sub normal", {
  validate:::name_sub(quote(.), quote(xyz), 2)
  validate:::name_sub(quote(.), quote(x + yz), 2)   # Works, but may break parent fun
  validate:::name_sub(quote(.), c(1:3), 2)          # Works, but may break parent fun
  validate:::name_sub(quote(..), quote(xyz), 2)
  validate:::name_sub(quote(...), quote(xyz), 2)
  validate:::name_sub(quote(.zzz), quote(xyz), 2)
  validate:::name_sub(quote(zzz.), quote(xyz), 2)
  validate:::name_sub(quote(zzz), quote(xyz), 2)
  validate:::name_sub(quote(a + b), quote(xyz), 2)  # Does nothing
  validate:::name_sub(quote(. + .), quote(xyz), 2)  # Does nothing
  validate:::name_sub(quote(.(zzz)), quote(xyz), 2) # Does nothing
  validate:::name_sub("hello", quote(xyz), 2)       # Does nothing
})
unitizer_sect("name_sub function", {
  validate:::name_sub(quote(.), quote(xyz), 1)
  validate:::name_sub(quote(.), quote(x + yz), 1)   # shouldn't matter
  validate:::name_sub(quote(.), c(1:3), 1)          # shouldn't matter
  validate:::name_sub(quote(..), quote(xyz), 1)
  validate:::name_sub(quote(...), quote(xyz), 1)
  validate:::name_sub(quote(.zzz), quote(xyz), 1)
  validate:::name_sub(quote(zzz.), quote(xyz), 1)
  validate:::name_sub(quote(zzz), quote(xyz), 1)
  validate:::name_sub(quote(a + b), quote(xyz), 1)  # Does nothing
  validate:::name_sub(quote(. + .), quote(xyz), 1)  # Does nothing
  validate:::name_sub(quote(.(zzz)), quote(xyz), 1) # Does nothing
  validate:::name_sub("hello", quote(xyz), 1)       # Does nothing
})
unitizer_sect("parse", {
  x <- quote(.(.))
  validate:::parse_validator(x, quote(arg_to_validate))
  x # make sure unchanged from previous assignment

  validate:::parse_validator(quote(FALSE), quote(arg_to_validate))
  validate:::parse_validator(quote(((FALSE))), quote(arg_to_validate))
  validate:::parse_validator(quote(.(FALSE)), quote(arg_to_validate))
  validate:::parse_validator(quote(.), quote(arg_to_validate))
  validate:::parse_validator(quote(. && a), quote(arg_to_validate))
  validate:::parse_validator(quote(.(.)), quote(arg_to_validate))
  validate:::parse_validator(quote(((a && b) || .(.))), quote(arg_to_validate))
  validate:::parse_validator(quote(matrix(nrow=3) && .(.)), quote(arg_to_validate))
  validate:::parse_validator(quote((a || ((b && c))) && .(a + .)), quote(arg_to_validate))
} )
