library(vetr)

unitizer_sect("All", {
  vetr:::val_all(1:10)  # -2
  vetr:::val_all(rep(TRUE, 10)) # 1
  vetr:::val_all(c(rep(TRUE, 10), FALSE, TRUE)) # 0
  vetr:::val_all(c(rep(TRUE, 5), NA, rep(TRUE, 5))) # -4
  vetr:::val_all(FALSE) # -1
  vetr:::val_all(TRUE)  # 2
  vetr:::val_all(logical()) # -5
  vetr:::val_all(NA)  # -3
  vetr:::val_all(c(TRUE, TRUE, NA, TRUE))  # -4
})

unitizer_sect("Hash", {
  # these should all equal 193
  vetr:::hash_fun(c("f b", "n b", "n d", "t m", "b r", "n w", "q w", "o x"))
})
