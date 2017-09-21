## Submission Checklist

* Review CRAN policy
* Check version
* Run tests with
    * winbuilder
    * valgrind
    * rchk
* Check coverage

## Submission Notes:

This is a resubmission of v0.2.0 to
fix CRAN farm failures:

* Fixes compilation failure on Solaris, we
  hope (removed `strnlen` fun, set
  `-D_POSIX_C_SOURCE=200112L` compilation flag
  in addition to `-std=c99 -pedantic` in our
  local checks).
* Set dependency to be R >= 3.3.2 to avoid
  failure on the CRAN osx old-rel machine
  that is still using that version of R
* Fixes some addition `rchck` errors that we
  somehow missed on the first pass.

## R CMD check --as-cran

Status: 1 NOTE

    Days since last update: 4

### Test Environments

Unfortunately I do not have access to a Solaris
test environment.  I have tested this package
against the following environments:

* Travis Ubuntu 14.04.5 LTS
    * R devel (2017-09-21 r73329)
    * R version 3.4.1 (2017-06-30)
    * R version 3.3.3 (2017-01-27)
* Winbuilder
    * R devel (2017-09-12 r73242):
      https://win-builder.r-project.org/Rm3eF0bZPfRI
    * R version 3.4.1 (2017-06-30)
      https://win-builder.r-project.org/eGej4A88Yq4U
* Locally on Mac OS 10.12.5
    * R version 3.4.1 (2017-06-30)
* Locally on a Ubuntu 14.04.5 LTS VM
    * R-devel (2017-09-13 r73255) with
      `--use-valgrind`
* Locally on a Ubuntu 16.04.2 LTS VM
    * Using the Kalibera rchck image on vagrant
    * `rchk` with R-devel (2017-09-21 r73329)

    * With env vars set for rcnst checks

