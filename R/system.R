## Remove DLLs when package is unloaded

# nocov start

.onUnload <- function(libpath) {
  library.dynam.unload("vetr", libpath)
}
# nocov end
