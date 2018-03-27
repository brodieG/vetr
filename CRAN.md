## Submission Checklist

* Review CRAN policy
* Check version
* Run tests with
    * winbuilder
    * valgrind
    * rchk
* Check coverage

## Submission Notes:

Maintenance release to fix some corner
case bugs and address CRAN notices.

## R CMD check --as-cran

Status: OK

### Test Environments

* rhub i386-pc-solaris2.10 (32-bit) (w/o vignettes):
    * R version 3.4.1 Patched (2017-07-15 r72924)
      http://builder.r-hub.io/status/vetr_0.2.3.tar.gz-d440589e2cdd44f989961f6c316fa324
* Travis Ubuntu 14.04.5 LTS
    * R devel (2018-03-01 r74329)
    * R version 3.4.2 (2017-01-27)
    * R version 3.3.3 (2017-01-27)
* Winbuilder
    * R devel (2018-03-01 r74337):
      https://win-builder.r-project.org/PbqdyR513msn
* Locally on Mac OS 10.12.5
    * R version 3.4.1 (2017-06-30)

