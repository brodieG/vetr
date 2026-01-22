## Submission Checklist

[x] Revdeps
[x] Review CRAN policy
[ ] Rebuild / Review doc changes
[x] Check version
[x] Run tests with
    [ ] winbuilder
[ ] Rhub
    [x] valgrind
    [x] rchk
    [ ] SAN
[x] Check coverage
[ ] Check build user
[ ] Revdeps

## Rhub

    rhub::rhub_platforms()
    rhub::rc_submit('vetr_0.2.20.tar.gz', c('valgrind', 'rchk', 'clang-asan'))
    rhub::rc_submit('vetr_0.2.20.tar.gz', c('rchk', 'clang-ubsan'))

We're not doing `gcc-asan` (no particular reason).


## Submission Notes:

This is a minor release primarily intended
to address the outstanding CRAN check warnings,
and fix a minor bug.

## R CMD check --as-cran

Completes with 'Status: OK'

## Test Environments

I have tested this package against the following
environments:

* Travis Ubuntu 14.04.5 LTS
    * R devel (2018-06-20 r74923)
    * R version 3.5.0 (2017-01-27)
    * R version 3.4.4 (2017-01-27)
    * R version 3.2.5 (2017-01-27)
* Winbuilder
    * R devel (2018-06-07 r74865)
      https://win-builder.r-project.org/T0GxmEvBfRne
* Locally Mac OS 10.13.5
    * R Version 3.5.0 (2017-01-27)

