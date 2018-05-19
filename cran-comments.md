## Resubmission
This is a resubmission. In this version I have:

* Removed Rglpk from **Imports** to **Suggests** in DESCRIPTION. lpSolveAPI has been added to **Suggests**. The reason is that the dependence on Rglpk causes the build to fail on OS X in the CRAN Checks. 

In the underlying code, I have implemented a solution which uses lpSolveAPI rather than Rglpk in cases where the user does not have Rglpk. Hence, users who do not have Rglpk can still use the package.

## Test environments
* local Windows 10 install, R 3.5.0
* local Ubuntu 16.04 install, R 3.4.4
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies of this package.
