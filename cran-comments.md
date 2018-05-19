## Resubmission
This is a resubmission. In this version I have:

* Removed Rglpk from **Imports** to **Suggests** in DESCRIPTION. lpSolveAPI has been added to **Suggests**. The reason is that the dependence on Rglpk causes the build to fail on OS X in the CRAN package Checks. In the underlying code, I have implemented a solution which uses lpSolveAPI rather than Rglpk in cases where the user does not have Rglpk. This should cause the package to pass the CRAN checks also for OS X, as it no longer import Rglpk.

## Test environments
* local OS X install, R 3.5.0
* local Windows 10 install, R 3.5.0
* local Ubuntu 16.04 install, R 3.4.4
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE from win-builder (release):

* checking CRAN incoming feasibility ... NOTE

Possibly mis-spelled words in DESCRIPTION:
  Dantzig (13:50)
  Sorensen (11:26, 14:30)
  al (11:38, 14:42)
  et (11:35, 14:39)

## Downstream dependencies
There are currently no downstream dependencies of this package.
