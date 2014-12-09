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
  validate:::name_sub("hello", quote(xyz), 1)       # Does nothing
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
  validate:::name_sub("hello", quote(xyz), 2)       # Does nothing
})
