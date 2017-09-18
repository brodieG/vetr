## Submission Checklist

* Review CRAN policy
* Check version
* Run tests with
    * winbuilder
    * valgrind
    * rchk
* Check coverage

## Submission Notes:

This is an update to an existing package that:

* Fixes CRAN errors caused by the no-attrs
  on symbols change
* Fixes valgrind, rchck, rcnst, ubsan, and
  other similar issues as reported by CRAN
  and Tomas Kalibera.
* Adds a few minor features and improvements

## R CMD check --as-cran

Status: OK

### Test Environments

I have tested this package against the following
environments:

* Travis Ubuntu 14.04.5 LTS
    * R devel (2017-09-15 r73276)
    * R version 3.4.1 (2017-06-30)
    * R version 3.3.3 (2017-01-27)
* Winbuilder
    * R devel (2017-09-12 r73242):
      https://win-builder.r-project.org/Jd0G7Argtsd1
    * R version 3.4.1 (2017-06-30)
      https://win-builder.r-project.org/3lBvbbRk1DCJ
* Locally on Mac OS 10.12.5
    * R version 3.4.1 (2017-06-30)
    * With --use-valgrind
    * Using the Kalibera rchck image on vagrant
    * With env vars set for rcnst checks

