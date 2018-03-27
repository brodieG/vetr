cat(getwd(), "\n")
if(suppressWarnings(require('unitizer'))) {
  unitize_dir('unitizer', state='recommended')
} else {
  warning("Cannot run tests without package `unitizer`")
}

