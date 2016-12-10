library(validate)

set.seed(1)
unitizer_sect("Tokens Pass", {
  validate(INT.1, 1)
  validate(INT.1.POS, 1)
  validate(INT.1.NEG, -1)
  validate(INT.1.POS.STR, 1)
  validate(INT.1.NEG.STR, -1)
  validate(INT, -1:1)
  validate(INT.POS, 0:3)
  validate(INT.NEG, 0:-3)
  validate(INT.POS.STR, 1:3)
  validate(INT.NEG.STR, -(1:3))
  validate(NUM.1, 1.44)
  validate(NUM.1.POS, 1.44)
  validate(NUM.1.NEG, -1.44)
  validate(NUM, runif(5))
  validate(NUM.POS, runif(5))
  validate(NUM.NEG, -runif(5))
  validate(CHR.1, "hello")
  validate(CHR, letters)
  validate(CPX, 1:10 + .5i)
  validate(CPX.1, 1 + .5i)
  validate(LGL, c(TRUE, FALSE))
  validate(LGL.1, TRUE)
} )
unitizer_sect("Tokens Fail", {
  validate(INT.1, 1.2)
  validate(INT.1, 1:2)
  validate(INT.1, NA_integer_)
  validate(INT.1, Inf)
  validate(INT.1.POS, -1)
  validate(INT.1.POS, 1:2)
  validate(INT.1.NEG, 1)
  validate(INT.1.NEG, -(1:2))
  validate(INT.1.POS.STR, 0)
  validate(INT.1.NEG.STR, 0)
  validate(INT, c(-1:1, NA_integer_))
  validate(INT, letters)
  validate(INT.POS, -(1:3))
  validate(INT.NEG, 1:3)
  validate(INT.POS.STR, 0:3)
  validate(INT.NEG.STR, -(0:3))
  validate(NUM.1, 1.44 + 1:2)
  validate(NUM.1.POS, -runif(1) - 1)
  validate(NUM.1.NEG, runif(1) + 1)
  validate(NUM, c(NA_real_, 1))
  validate(NUM, NULL)
  validate(NUM.POS, -runif(5) - 1)
  validate(NUM.NEG, runif(5) + 1)
  validate(CHR.1, letters)
  validate(CHR, list(1, 2, 3))
  validate(CPX, list(1, 2, 3))
  validate(CPX.1, list(1, 2, 3))
  validate(LGL, NA)
  validate(LGL, letters)
  validate(LGL.1, 1:2 == 1:2)
})
unitizer_sect("Compound Expressions", {
  validate(INT.1 || NULL, 1)    # Pass
  validate(INT.1 || NULL, NULL) # Pass
  validate(INT.1 || NULL, 1.4)  # Fail

  validate((matrix(integer(), 0) && nrow(.) == ncol(.)) || NULL, matrix(1:16, nrow=4))  # Pass
  validate((matrix(integer(), 0) && nrow(.) == ncol(.)) || NULL, matrix(1:16, nrow=2))  # Fail
  validate((matrix(integer(), 0) && nrow(.) == ncol(.)) || NULL, matrix(runif(16), nrow=4))  # Fail
})

unitizer_sect("Other Return Modes", {
  validate(INT.1 || NULL || LGL, "hello", return.mode="text")
  validate(INT.1 || NULL || LGL, "hello", return.mode="raw")
  validate(INT.1 || NULL || LGL, "hello", return.mode="full")
})

unitizer_sect("Multi-line Stuff", {
  # with a validator with message attached
  validate(
    NO.NA,
    c(234234131431, 123413413413, 1341341341, 12341234134, 562456234, 24624624,
      2452345234, 2345234524, 23452452, 2243524352, 254254234, 2452452435, NA)
  )
  # No message
  validate(
    quote(!anyNA(.)),
    c(234234131431, 123413413413, 1341341341, 12341234134, 562456234, 24624624,
      2452345234, 2345234524, 23452452, 2243524352, 254254234, 2452452435, NA)
  )
  validate(quote(!anyNA(.)), c(234234131431, 123413413413, NA))
})
