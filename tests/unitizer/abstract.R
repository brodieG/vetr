library(alike)

unitizer_sect("Standard Methods", {
  abstract(1:10)
  abstract(list(a=1:10, b=runif(10)))
  abstract(matrix(1:9, nrow=3))
  abstract(
    array(1:8, c(2, 2, 2), dimnames=list(letters[1:2], LETTERS[1:2], NULL))
  )
  alike(abstract(iris), iris[1:10, ])
  alike(abstract(iris), iris[1:10, 1:3])
  alike(abstract(iris), transform(iris, Species=as.character(Species)))
})
unitizer_sect("Time Series", {
  y <- ts(runif(12), start=1970, freq=12)
  attr(abstract(y), "ts")
  attr(abstract(y, "start"), "ts")
  attr(abstract(y, "end"), "ts")
  attr(abstract(y, "frequency"), "ts")
  attr(abstract(y, c("start", "frequency")), "ts")

  # Errors

  abstract(y, "boom")
  alike:::abstract.ts(1:12)
})
unitizer_sect("lm", {
  set.seed(1)
  df1 <- data.frame(x = runif(10), y=runif(10), z=runif(10))
  df2 <- data.frame(a = runif(5), b=runif(5), c=runif(5))
  mdl <- lm(y ~ x + poly(z, 2), df1)

  alike(abstract(mdl), mdl)

  mdl2 <- lm(x ~ y + poly(z, 2), df1)

  alike(abstract(mdl), mdl2)

  mdl3 <- lm(a ~ b + log(c), df2)

  alike(abstract(mdl), mdl3)

  mdl4 <- lm(a ~ b, df2)

  alike(abstract(mdl), mdl4)
})
unitizer_sect("ggplot", {
  # Rather experimental
  library(ggplot2)
  df1 <- data.frame(x=runif(20), y=runif(20))
  df2 <- data.frame(x=runif(20), y=runif(20), z=rep(c("a", "b"), 10))
  df3 <- data.frame(a=runif(30), b=runif(30))
  g1 <- ggplot(df1) + geom_point(aes(x=x, y=y))
  g2 <- ggplot(df1) + geom_line(aes(x=x, y=y))
  g3 <- ggplot(df3) + geom_point(aes(x=a, y=b))
  g4 <- ggplot(df1, aes(x=x, y=y)) + geom_point() + geom_line()
  g.abs <- abstract(g1)

  alike(g.abs, g1)
  alike(g.abs, g2)
  alike(g.abs, g3)
})
unitizer_sect("nullify", {
  nullify(list(1, 2, 3), 2)
  nullify(list(1, 2, 3), -2)
  nullify(list(1, 2, 3, 4), c(TRUE, FALSE))
  nullify(list(1, 2, 3, 4), c(TRUE, FALSE, FALSE))
  nullify(list(1, 2, 3, 4), c(rep(FALSE, 4), TRUE))
  nullify(list(a=1, b=2, 3, 4), c("a", "b"))
  nullify(list(1, 2, 3, 4), "hello")

  nullify(list(1, 2, 3), 4)
  nullify(iris[1:10, ], 4)

  nullify(letters, 5)

  nullify(structure(letters[1:2], class='xqwer892jahaksdf'), 2)
})
