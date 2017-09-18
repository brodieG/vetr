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

* Remove use of `strnlen` which caused solaris
  build to fail
* Set dependency to be R >= 3.3.2 to avoid
  failure on the CRAN osx old-rel machine
  that is still using that version of R

## R CMD check --as-cran

Status: 1 NOTE (Days since last update)

### Test Environments

I have tested this package against the following
environments:

* Travis Ubuntu 14.04.5 LTS
    * R devel (2017-09-18 r73313)
    * R version 3.4.1 (2017-06-30)
    * R version 3.3.3 (2017-01-27)
* Winbuilder
    * R devel (2017-09-12 r73242):
      https://win-builder.r-project.org/Rm3eF0bZPfRI
    * R version 3.4.1 (2017-06-30)
      https://win-builder.r-project.org/eGej4A88Yq4U
* Locally on Mac OS 10.12.5
    * R version 3.4.1 (2017-06-30)


    * With --use-valgrind
    * Using the Kalibera rchck image on vagrant
    * With env vars set for rcnst checks

