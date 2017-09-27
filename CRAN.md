## Submission Checklist

* Review CRAN policy
* Check version
* Run tests with
    * winbuilder
    * valgrind
    * rchk
* Check coverage

## Submission Notes:

This is a resubmission to fix the failing 
Solaris CRAN farm check.  I apologize for 
resubmitting so soon.  This time I tested
on rhub Solaris.


## R CMD check --as-cran

Status: 1 NOTE

    Days since last update

### Test Environments

Much to my delight I discovered `rhub` has
a Solaris test environment.  I have tested
this package on the following:

* rhub i386-pc-solaris2.10 (32-bit):
    * R version 3.4.1 Patched (2017-07-15 r72924)
* Travis Ubuntu 14.04.5 LTS
    * R devel (2017-09-22 r73336)
    * R version 3.4.1 (2017-06-30)
    * R version 3.3.3 (2017-01-27)
* Winbuilder
    * R devel (2017-09-12 r73242):
      https://win-builder.r-project.org/ahP9elX0NRy1
    * R version 3.4.1 (2017-06-30)
      https://win-builder.r-project.org/sShYVv2BduzF/
* Locally on Mac OS 10.12.5
    * R version 3.4.1 (2017-06-30)

The previous version was also checked against
the following, but since changes are minor I did
not re-run these:

* Locally on a Ubuntu 14.04.5 LTS VM
    * R-devel (2017-09-13 r73255) with
      `--use-valgrind`
* Locally on a Ubuntu 16.04.2 LTS VM
    * Using the Kalibera rchck image on vagrant
    * `rchk` with R-devel (2017-09-21 r73329)
    * With env vars set for rcnst checks

