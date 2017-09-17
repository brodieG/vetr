## Submission Checklist

* Review CRAN policy
* Check version
* Run tests with
    * winbuilder
    * valgrind
    * rchk
* Check coverage

## Submission Notes:

This is an update to an existing package
that fixes valgrind, rchck, rcnst, and other
similar issues.  Additionally, the test that
set attributes on a symbol has been removed
which fixes the newly failing tests on CRAN.

Some new features and improvements are
included too.

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
    * R devel (2017-07-05 r72891):
      https://win-builder.r-project.org/
* Locally on Mac OS 10.12.5
    * R version 3.4.1 (2017-06-30)
    * With --use-valgrind
    * Using the Kalibera rchck image on vagrant
    * With env vars set for rcnst checks

