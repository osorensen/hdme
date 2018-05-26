# hdme 0.2.0.9000
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
