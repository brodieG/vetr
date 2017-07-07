library(vetr)

unitizer_sect("Class Matching", {
  obj2 <- structure(numeric())
  obj1 <- structure(numeric(), class="hello")
  alike(obj1, obj2)
  obj2 <- structure(numeric(), class=c(letters[10:12], letters[1:3], letters[8:9]))
  obj1 <- structure(numeric(), class=letters[1:3])
  alike(obj1, obj2)
  alike(obj2, obj1)
  obj2 <- structure(numeric(), class=c("b", "a", "c"))
  alike(obj1, obj2)
  obj2 <- structure(numeric(), class=c("a", "b", "x", "c"))
  alike(obj1, obj2)
  obj2 <- structure(numeric(), class=c("a", "b", "c"))
  alike(obj1, obj2)      # TRUE
  obj2 <- structure(numeric(), class=c("x", "a", "b", "c"))
  alike(obj1, obj2)      # TRUE
  alike(obj1, obj2, settings=vetr_settings(attr.mode=1))   # FALSE
} )
unitizer_sect("S4", {
  bn <- .BaseNamespaceEnv
  setClass("foo", representation(a = "character", b = "numeric"), where=bn)
  setClass("bar", representation(d = "numeric", c = "numeric"), where=bn)
  setClass("baz", contains="foo", list(c="character"), where=bn)

  x <- new("foo")
  y <- new("foo")
  z <- new("bar")
  v <- new("baz")
  w <- structure(list(a=character(), b=numeric()), class="foo")

  alike(x, y)  # TRUE
  alike(x, z)  # FALSE
  alike(x, w)  # FALSE
  alike(w, x)  # FALSE
  alike(x, v)  # TRUE, because v contains x
  alike(v, x)  # FALSE

  # S4 nested in list

  lst.2 <- list(list(11, 21), list(31, list(41, list(51, list(61)))))
  lst.5 <- lst.6 <- lst.2
  lst.5[[2]][[2]][[1]] <- x
  lst.6[[2]][[2]][[1]] <- v

  alike(lst.5, lst.6)  # TRUE
  alike(lst.6, lst.5)  # FALSE, child class is target, so parent can't match

  # Borked S4
  v2 <- v
  class(v2) <- c("baz", "foo")
  alike(x, v2)

  # Stress test installation of `inherits`; right now the inherits command is
  # evaluated in the base environment, which seems to work since the arguments
  # are already evaluated, but it suggests `inherits` can look up S4 definitions
  # irrespective of where they are defined...

  inherits <- function(x, y) stop("pwned!!!")
  alike(y, v)  # TRUE
} )
unitizer_sect("R5", {
  Foo <- setRefClass("Foo", where=bn)
  Bar <- setRefClass("Bar", where=bn)

  Foo.1 <- Foo$new()
  Foo.2 <- Foo$new()
  Bar.1 <- Bar$new()

  alike(Foo.1, Foo.2)
  alike(Foo.1, Bar.1)

} )
unitizer_sect("Non-Standard Class", {
  # Basically ensure that stuff still recurses even if they are lists/calls
  # but have another class

  var.1 <- list(1, 2, 3)
  var.2 <- list("hello", list(1, 2, 3), 5)
  class(var.1) <- "marbles"
  class(var.2) <- "marbles"
  # "mis-match at index [[1]]: should be integer instead of character"
  alike(var.1, var.2)
} )
