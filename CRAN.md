## Submission Checklist

[x] Review CRAN policy
[x] Rebuild / Review doc changes
[x] Check version
[x] Run tests with
    [x] winbuilder https://win-builder.r-project.org/upload.aspx
    [x] GA R-devel
    [x] GA R-oldrel
    [x] GA R-rel
    [x] Rhub valgrind
    [x] Rhub rchk
    [x] Rhub SAN
[x] Check coverage
[x] Revdeps
    [ ] Dataonderivates, make sure to run with NOT_CRAN=false in ~/.Renviron

## Rhub

    rhub::rhub_platforms()
    rhub::rc_submit('vetr_0.2.21.tar.gz', c('valgrind', 'rchk', 'clang-asan'))

ASAN runs ubsan.


## Submission Notes:

This is a minor release primarily intended
to address the outstanding CRAN check warnings,
and fix a minor bug.

## R CMD check --as-cran

Completes with 'Status: OK'
