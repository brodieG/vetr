library(validate)

unitizer_sect("All", {
  validate:::val_all(1:10)  # -2
  validate:::val_all(rep(TRUE, 10)) # 1
  validate:::val_all(c(rep(TRUE, 10), FALSE, TRUE)) # 0
  validate:::val_all(c(rep(TRUE, 5), NA, rep(TRUE, 5))) # -3
  validate:::val_all(FALSE) # -1
  validate:::val_all(TRUE)  # 2
  validate:::val_all(logical()) # -4
  validate:::val_all(NA)  # -3
  validate:::val_all(c(TRUE, TRUE, NA, TRUE))  # -3
})
