cat(getwd(), "\n")
if(suppressWarnings(require('unitizer'))) {
  RNGversion("3.5.2");
  unitize_dir('unitizer', state='recommended')
  RNGversion(as.character(getRversion()))
} else {
  warning("Cannot run tests without package `unitizer`")
}

