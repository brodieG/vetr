library(vetr)

unitizer_sect('all_bw', {
  x <- runif(100)
  x[1] <- 1
  x[2] <- 0
  all_bw(x, 0, 1)
  all_bw(x, 0, 1, include.bounds="[)") # fail
  all_bw(x, 0, 1, include.bounds="(]") # fail
  all_bw(x, 0, 1, include.bounds="()") # fail

  all_bw(x, 0, 1 + 1e-6, include.bounds="[)") # pass
  all_bw(x, 0 - 1e-6, 1, include.bounds="(]") # pass
  all_bw(x, 0 - 1e-6, 1 + 1e-6, include.bounds="()") # pass

  y <- z <- x
  y[50] <- NA
  z[50] <- NaN

  all_bw(y, 0, 1)              # fail
  all_bw(y, 0, 1, na.rm=TRUE)  # pass

  all_bw(z, 0, 1)              # fail
  all_bw(z, 0, 1, na.rm=TRUE)  # pass
})
