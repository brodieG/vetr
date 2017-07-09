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
unitizer_sect("bench_mark", {
  # three different time frames that should trigger all the code, need to remove
  # the time piece so that the tests don't fail due to variations

  capt_wo_time <- function(x) {
    txt <- capture.output(x)
    gsub("~ *-?[0-9.]*", "~", txt)
  }
  capt_wo_time(bench_mark(Sys.sleep(1.2), times=1))
  capt_wo_time(bench_mark(Sys.sleep(.01), times=10))
  capt_wo_time(bench_mark(1 + 1, NULL, times=100))
})
