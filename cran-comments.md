## Resubmission
This is a resubmission, with the goal of fixing an issue related to random numbers in the development version of R. The package currently on CRAN has a test that relies on the random number seed, which fails on r-devel, as pointed out in an e-mail from Knut Hornik. The issue was first reproduced on R-hub, and then fixed.

## Test environments
* local os X install, R 3.5.2
* win-builder (devel and release)
* R-hub (devel)

## R CMD check results
There were no ERRORs or WARNINGs.

There was one NOTE on R-hub (devel):

  * checking package dependencies ... NOTE
  Package suggested but not available for checking: Rglpk

## Downstream dependencies
There are currently no downstream dependencies of this package.
