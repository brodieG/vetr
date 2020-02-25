cat(getwd(), "\n")
if(suppressWarnings(require('unitizer'))) {
  RNGversion("3.5.2");
  pattern <- 'alike'
  unitize_dir(
    'unitizer',
    # pattern=pattern,
    state='recommended'
  )
  RNGversion(as.character(getRversion()))
} else {
  warning("Cannot run tests without package `unitizer`")
}

