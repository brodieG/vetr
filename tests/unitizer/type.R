library(vetr)

# Redefine names; necessary because we renamed functions when moving to C
# versions

unitizer_sect("type_of", {
  type_of(1:100)
  type_of(1.1)
  type_of(1:100 + 1.0)
  type_of(1:100 + 1/1e9)
  type_of(NA_real_)
  type_of(Inf)
  type_of(-Inf)
} )
unitizer_sect("type_alike", {
  type_alike(1, 1.1)          # TRUE, 1 is numeric
  type_alike(1L, 1.1)         # FALSE
  type_alike(1L, 1.00000001)  # FALSE
  type_alike(1L, 1.0)         # TRUE

  type_alike(1, 1.1, vetr_settings(type.mode=1))   # TRUE, 1 is numeric
  type_alike(1L, 1.0, vetr_settings(type.mode=1))  # FALSE
  type_alike(1.0, 1L, vetr_settings(type.mode=1))  # TRUE
  type_alike(1.0, 1L, vetr_settings(type.mode=2))  # FALSE, must be num-num

  type_alike(1:100, 1:100 + 0.0)  # TRUE
  type_alike(1:101, 1:101 + 0.0)  # FALSE
  type_alike(1:101, 1:101 + 0.0, vetr_settings(fuzzy.int.max.len=200))  # TRUE

  type_alike(numeric(), c(1.1, 0.053, 41.8))  # TRUE
  type_alike(numeric(), list(1.1))  # FALSE
  type_alike(list(), integer())     # FALSE
  type_alike(1000000L, 1000000L + .1)    # FALSE
  type_alike(1000000L, 1000000L + .0)    # TRUE
  type_alike(data.frame(a=1:10), list()) # TRUE
  type_alike(NULL, NULL)
  type_alike(1/0, NA)

  # errors

  type_alike(1, 1.1, vetr_settings(type.mode=1:2))
  type_alike(1, 1.1, vetr_settings(fuzzy.int.max.len=1:2))
} )
unitizer_sect("functions", {
  type_alike(sd, var)     # clo-clo
  type_alike(`&&`, sd)    # spe-clo
  type_alike(`&&`, sum)   # spe-blt
  type_alike(sum, sd)     # blt-clo
  type_alike(sum, c)      # blt-blt
  type_alike(`&&`, `[`)   # spe-spe

  type_alike(sd, 1:3)

  type_alike(sd, var, vetr_settings(type.mode=1))     # clo-clo
  type_alike(`&&`, sd, vetr_settings(type.mode=1))    # spe-clo
  type_alike(`&&`, sum, vetr_settings(type.mode=1))   # spe-blt
  type_alike(sum, sd, vetr_settings(type.mode=1))     # blt-clo
  type_alike(sum, c, vetr_settings(type.mode=1))      # blt-blt
  type_alike(`&&`, `[`, vetr_settings(type.mode=1))   # spe-spe
} )
