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
similar issues.  Some new features and
improvements are included too.

## R CMD check --as-cran

Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Brodie Gaslam <brodie.gaslam@yahoo.com>’

New submission

### Test Environments

I have tested this package against the following
environments:

* Travis Ubuntu 12.04.5 LTS
    * R devel (2017-07-06 r72894)
    * R version 3.4.0 (2017-04-21)
    * R version 3.3.3 (2017-03-06)
* Winbuilder
    * R devel (2017-07-05 r72891): https://win-builder.r-project.org/A467DXNN6hxP
* Locally on Mac OS 10.12.5
    * R version 3.4.1 (2017-06-30)


