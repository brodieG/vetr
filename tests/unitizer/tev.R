library(vetr)

unitizer_sect("tev", {
  tev(runif(2), numeric(2))
  tev(runif(3), numeric(2))

  # # we can no longer do this without including magrittr in suggests
  # has.magrittr <- suppressWarnings(require(magrittr, quietly=TRUE))

  # if(has.magrittr) runif(2) %>% tev(numeric(2)) %>% isTRUE else TRUE
  # if(has.magrittr) runif(3) %>% tev(numeric(2)) %>% isTRUE else FALSE
})
