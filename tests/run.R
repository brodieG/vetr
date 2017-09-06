library(unitizer)
# env R_COMPILE_PKGS=1 R CMD build .
# env R_JIT_STRATEGY=4 R_CHECK_CONSTANTS=5 R CMD check vetr_0.1.0.9008.tar.gz
# env R_COMPILE_PKGS=1 R_JIT_STRATEGY=4 R_CHECK_CONSTANTS=5 R
unitize_dir('unitizer', state='recommended')
