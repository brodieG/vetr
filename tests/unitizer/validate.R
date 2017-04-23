library(vetr)

set.seed(1)
unitizer_sect("Tokens Pass", {
  vet(INT.1, 1)
  vet(INT.1.POS, 1)
  vet(INT.1.NEG, -1)
  vet(INT.1.POS.STR, 1)
  vet(INT.1.NEG.STR, -1)
  vet(INT, -1:1)
  vet(INT.POS, 0:3)
  vet(INT.NEG, 0:-3)
  vet(INT.POS.STR, 1:3)
  vet(INT.NEG.STR, -(1:3))
  vet(NUM.1, 1.44)
  vet(NUM.1.POS, 1.44)
  vet(NUM.1.NEG, -1.44)
  vet(NUM, runif(5))
  vet(NUM.POS, runif(5))
  vet(NUM.NEG, -runif(5))
  vet(CHR.1, "hello")
  vet(CHR, letters)
  vet(CPX, 1:10 + .5i)
  vet(CPX.1, 1 + .5i)
  vet(LGL, c(TRUE, FALSE))
  vet(LGL.1, TRUE)
} )
unitizer_sect("Tokens Fail", {
  vet(INT.1, 1.2)
  vet(INT.1, 1:2)
  vet(INT.1, NA_integer_)
  vet(INT.1, Inf)
  vet(INT.1.POS, -1)
  vet(INT.1.POS, 1:2)
  vet(INT.1.NEG, 1)
  vet(INT.1.NEG, -(1:2))
  vet(INT.1.POS.STR, 0)
  vet(INT.1.NEG.STR, 0)
  vet(INT, c(-1:1, NA_integer_))
  vet(INT, letters)
  vet(INT.POS, -(1:3))
  vet(INT.NEG, 1:3)
  vet(INT.POS.STR, 0:3)
  vet(INT.NEG.STR, -(0:3))
  vet(NUM.1, 1.44 + 1:2)
  vet(NUM.1.POS, -runif(1) - 1)
  vet(NUM.1.NEG, runif(1) + 1)
  vet(NUM, c(NA_real_, 1))
  vet(NUM, NULL)
  vet(NUM.POS, -runif(5) - 1)
  vet(NUM.NEG, runif(5) + 1)
  vet(CHR.1, letters)
  vet(CHR, list(1, 2, 3))
  vet(CPX, list(1, 2, 3))
  vet(CPX.1, list(1, 2, 3))
  vet(LGL, NA)
  vet(LGL, letters)
  vet(LGL.1, 1:2 == 1:2)
})
unitizer_sect("Compound Expressions", {
  vet(INT.1 || NULL, 1)    # Pass
  vet(INT.1 || NULL, NULL) # Pass
  vet(INT.1 || NULL, 1.4)  # Fail
  vet(INT.1 || NULL || character(3L), 1)    # Pass
  vet(INT.1 || NULL || character(3L), 1.2)  # Fail
  vet(INT.1 || NULL || character(3L), letters)  # Fail

  # Pass

  vet(
    (matrix(integer(), 0) && nrow(.) == ncol(.)) || NULL,
    matrix(1:16, nrow=4)
  )
  # Fail

  vet(
    (matrix(integer(), 0) && nrow(.) == ncol(.)) || NULL,
    matrix(1:16, nrow=2)
  )
  # Fail

  vet(
    (matrix(integer(), 0) && nrow(.) == ncol(.)) || NULL,
    matrix(runif(16), nrow=4)
  )
})

unitizer_sect("Other Return Modes", {
  vet(INT.1 || NULL || LGL, "hello", format="text")
  vet(INT.1 || NULL || LGL, "hello", format="raw")
  vet(INT.1 || NULL || LGL, "hello", format="full")
})

unitizer_sect("Multi-line Stuff", {
  # with a validator with message attached
  vet(
    NO.NA,
    c(234234131431, 123413413413, 1341341341, 12341234134, 562456234, 24624624,
      2452345234, 2345234524, 23452452, 2243524352, 254254234, 2452452435, NA)
  )
  vet(
    NO.NA || !anyNA(.),
    c(234234131431, 123413413413, 1341341341, 12341234134, 562456234, 24624624,
      2452345234, 2345234524, 23452452, 2243524352, 254254234, 2452452435, NA)
  )
  # No message
  vet(
    !anyNA(.),
    c(234234131431, 123413413413, 1341341341, 12341234134, 562456234, 24624624,
      2452345234, 2345234524, 23452452, 2243524352, 254254234, 2452452435, NA)
  )
  vet(!anyNA(.), c(234234131431, 123413413413, NA))

  # stored validation

  val.exp <- quote(!anyNA(.))
  vet(val.exp, c(234234131431, 123413413413, NA))
})

unitizer_sect("Language", {
  # Note issue #18; not 100% sure this is correct, actually it should be, the
  # validator expression is always substituted, and any symbols pointing to
  # language are used as language.
  vet(quote(quote(a + b)), quote(x2 + x3))
  x <- quote(quote(a + b))
  vet(x, quote(x2 + x3))
  vet(quote(a + b), quote(2 + x3))
  vet(quote(a + b), quote(x1 + x2 + x3))
})
