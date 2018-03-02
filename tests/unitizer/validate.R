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
  vet(CHR, character())
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
unitizer_sect("Custom expressions", {
  vet(. > 5, 1:10)
  vet(. > 5, 6:10)

  # corner cases

  vet(.(c(TRUE, NA, TRUE)), 1:5)
  vet(.(1:5), 1:5)
  vet(.(1:5, 1:5), 1:5)      # error
  vet(.(list(1, 2, 3)), 1:3) # error

  vet(.(c('hello world', 'goodbye moon')), 1:3)
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
  # custom expression partially defined in parent env

  exp.a <- quote(all(. > 0))
  exp.b <- quote(is.vector(.))

  vet(exp.a && exp.b, -(1:3))

  # some testing of nesting, this could conflict with prior exp.a
  # if not done properly

  local({
    exp.a <- quote(all(. < 0))
    vet(exp.a, -(1:3))
  })
  # Duplicate expressions should get collapsed in error message

  vet(1 || "a" || 1 || "a" || 1 || letters, 1:3)
})

unitizer_sect("Other Return Modes", {
  vet(INT.1 || NULL || LGL, "hello", format="text")
  vet(INT.1 || NULL || LGL, "hello", format="raw")
  vet(INT.1 || NULL || LGL, "hello", format="full")
  vet(INT.1 || NULL || LGL, "hello", format="halloween")
  vet(INT.1 || NULL || LGL, "hello", format=1:10)

  vet(INT.1 || NULL || LGL, "hello", format="text", stop=TRUE)
  vet(INT.1 || NULL || LGL, "hello", format="text", stop=1:3)
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
unitizer_sect("Embedded String Errors", {
  vet(all_bw(., 0, 1), 0:5)
  vet(all.equal(., 1:5), 1:6)
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

  # Test recursive substitution across environments, first check that all the
  # symbols we use don't actually exist currently

  unlist(lapply(c('aaA', 'bbB', 'ccC', 'ddD', 'eeE'), find))# should be length 0

  x <- quote(aaA + bbB)
  my.env <- new.env()
  my.env$y <- quote(ccC - ddD)

  # FALSE because `x` is expanded

  evalq(vet(quote(x * y), quote(A * (B - C))), envir=my.env)

  # TRUE because `eeE` is not expanded (but `y` is)

  evalq(vet(quote(eeE * y), quote(A * (B - C))), envir=my.env)

  # TRUE because expansion matches

  evalq(vet(quote(x * y), quote((A + D) * (B - C))), envir=my.env)

  # potentialy infinite recursion

  expA <- expB <- expC <- expD <- expE <- 0
  expA <- quote(expB && expC)
  expB <- quote(expD * expE)
  expE <- quote(expA || expD)

  vet(expA, TRUE)

  # check that symbols (i.e. not call) are resolved recursively too

  expE <- quote(expA)
  vet(expA, TRUE)

  # Check that `..` is expanded properly

  . <- quote(. > 0)
  vet(.., 1.4)
  . <- quote(numeric(1L))
  vet(.., 1.5)
})
unitizer_sect("Errors", {
  vet(1, 1, env="hello")
})

unitizer_sect("Custom tokens", {
  cust.tok.1 <- vet_token(quote(TRUE), "%sshould be logical(1L)")

  vet(cust.tok.1, TRUE)
  vet(cust.tok.1, 1:2)

  # impossible tokens

  vet_token(quote(TRUE), "should be logical(1L)")
  vet_token(quote(TRUE), letters)

  # hack impossile token (`vet_token` itself wont allow it)

  cust.tok.2 <- quote(. > 2)
  attr(cust.tok.2, "err.msg") <- letters

  vet(cust.tok.2, TRUE)
})

unitizer_sect("Result Buffer", {
  # testing that result buffer expands correctly

  set1 <- vetr_settings(result.list.size.init=1)

  vet.exp <- quote(1 || 1:2 || 1:3 || 1:4 || 1:5 || 1:6 || 1:7 || 1:8)

  vet(vet.exp, 1:8, settings=set1)
  vet(vet.exp, 1:9, settings=set1)

  set2 <- vetr_settings(result.list.size.init=1, result.list.size.max=7)

  vet(vet.exp, 1:8, settings=set2)
  vet(vet.exp, 1:9, settings=set2)

  set3 <- vetr_settings(result.list.size.init=1, result.list.size.max=8)

  vet(vet.exp, 1:8, settings=set3)
  vet(vet.exp, 1:9, settings=set3)

  # impossible settings

  set4 <- vetr_settings(result.list.size.init="hello", result.list.size.max=8)
  set5 <- vetr_settings(result.list.size.init=1, result.list.size.max="hello")

  vet(1, 1, settings=set4)
  vet(1, 1, settings=set5)
})
