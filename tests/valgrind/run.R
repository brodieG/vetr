# overwrite unitizer_sect to just plain eval stuff

unitizer_sect <- function(
  title = NULL, expr=expression(), details=character(), compare=identical
) {
  expr.sub <- substitute(expr)
  for(i in as.list(tail(expr.sub, -1L))) try(eval(i, envir=parent.frame()))
}

source('unitizer/_pre/lorem.R')
source('unitizer/alike.R', echo=TRUE)
source('unitizer/cstringr.R', echo=TRUE)
source('unitizer/language.R', echo=TRUE)
source('unitizer/validate.R', echo=TRUE)
