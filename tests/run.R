cat(getwd(), "\n")
if(suppressWarnings(require('unitizer'))) {
  local({
    suppressWarnings(RNGversion("3.5.2"));
    old.opt <- options(stringsAsFactors=TRUE)
    on.exit({
      RNGversion(as.character(getRversion()))
      options(old.opt)
    })
    pattern <- 'alike'
    unitize_dir(
      'unitizer',
      # pattern=pattern,
      state='recommended'
    )
  })
} else {
  warning("Cannot run tests without package `unitizer`")
}

