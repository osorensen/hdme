# hdme 0.5.1

- Colorblind friendly plots in vignette. Thanks to Andre Cruz for suggesting.
- Removed C++11 flags from Makevars and Makevars.win.

# hdme 0.5.0

- Fixed bug in stopping condition for projected gradient descent. Thanks to Michael
Pollmann for discovering this.
- Fixed numerical overflow issue which happened for corrected lasso with Poisson regression.

# hdme 0.4.0

- Fixes a bug in `corrected_lasso()` with the `family = "poisson"` option. Thanks
  to Huang Jun for discovering this.
- Corrected a logical error in the examples, in which the variance of the measurement 
  errors was supplied as the standard error.
  

# hdme 0.3.4

- Changed to Authors(at)R notation in DESCRIPTION.

# hdme 0.3.3

- Package 'flare' removed from suggested dependencies.

# hdme 0.3.2

- Changed used of `coef` function from `glmnet`, due to a breaking change in `glmnet`.

# hdme 0.3.1.9000

- New function `cv_gds` which computes cross-validated generalized Dantzig selector.

# hdme 0.3.1

- Added case weighting to `gds` and `gmus`.

# hdme 0.3.0

- This release contains all changes since 0.2.3, as listed under the headings below.

# hdme 0.2.3.9001

## S3 methods

- Added coef methods `coef.corrected_lasso`, `coef.gds`, `coef.gmu_lasso`, and `coef.gmus`. 
- Added print methods `print.corrected_lasso`, `print.cv_corrected_lasso`, `print.gds`, `print.gmu_lasso`, and `print.gmus`.
- Improved `plot.corrected_lasso` to also handle the case of a single regularization parameter.

## Dependencies

- Figured out how to install `Rglpk` on travis CI, so now this package is back in **Imports**, and `lpSolveAPI` is no longer used.

# hdme 0.2.3.9000
- It turns out that travis CI is not able to install `Rglpk`. Hence, for the development version `Rglpk` is back in **Suggests**, together with `lpSolveAPI`.

# hdme 0.2.3
- Added `Rglpk` back to **Imports** and removed `lpSolveAPI`, as the latest version of `Rglpk` passes all tests on CRAN, including osX.
- Updated vignette.
- Cleaned up documentation, hiding internal functions from the index.
- Created unit tests.

# hdme 0.2.2
Fixed random number seed issue which caused test to fail in R-devel.

# hdme 0.2.1
Internal fix to `fit_corrected_lasso` and `cv_corrected_lasso`. Got rid of duplicated code by calling the function `set_radius`.

# hdme 0.2.0
Since `Rglpk` does not install automatically on macOS, this package was moved from **Imports** to **Suggests**. In addition, `lpSolveAPI` was added to **Suggests**. This means that the package should build also on systems that do not have `Rglpk`, in particular the versions of macOS on CRAN.

The changes involved adding an optional linear solver from `lpSolveAPI` in the function `musalgorithm()`.

# hdme 0.1.1.9002
Added a `plot.gds` function, which plots the coefficients estimated by `fit_gds`.

# hdme 0.1.1.9001
Internal adjustment, which removed importing of external packages into the namespace, and instead specified the functions explicitly using `::`.

# hdme 0.1.1
`tidyverse` has been removed from **Suggests** field `DESCRIPTION`. `dplyr` and `tidyr` have been added instead. Similarly, `library(tidyverse)` in the vignette has been replaced by `library(dplyr)`, `library(tidyr)`, and `library(ggplot2)`.

# hdme 0.1.0
`hdme` is now on CRAN.
