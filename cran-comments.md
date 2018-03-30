## Resubmission
This is a resubmission. In this version I have:

* Removed tidyverse from the Suggests field of DESCRIPTION, and instead added explicitly the packages dplyr and tidyr.
* Replaced library(tidyverse) in the vignette with library(dplyr), library(tidyr), and library(ggplot2).

## Test environments
* local OS X install, R 3.4.4
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
