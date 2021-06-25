
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hdme

<!-- badges: start -->

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/hdme)](https://cran.r-project.org/package=hdme)
[![codecov](https://codecov.io/gh/osorensen/hdme/branch/master/graph/badge.svg)](https://codecov.io/gh/osorensen/hdme)
[![DOI](http://joss.theoj.org/papers/10.21105/joss.01404/status.svg)](https://doi.org/10.21105/joss.01404)
[![R-CMD-check](https://github.com/osorensen/hdme/workflows/R-CMD-check/badge.svg)](https://github.com/osorensen/hdme/actions)
<!-- badges: end -->

The goal of hdme is to provide penalized regression methods for
High-Dimensional Measurement Error problems (errors-in-variables).

## Installation

Install `hdme` from CRAN using.

``` r
install.packages("hdme")
```

You can install the latest development version from github with:

``` r
# install.packages("devtools")
devtools::install_github("osorensen/hdme", build_vignettes = TRUE)
```

### Dependency on Rglpk

`hdme` uses the [Rglpk
package](https://cran.r-project.org/package=Rglpk), which requires the
GLPK library package to be installed. On some platforms this requires a
manual installation.

On Debian/Ubuntu, you might use:

``` sh
sudo apt-get install libglpk-dev
```

On macOS, you might use:

``` sh
brew install glpk
```

## Methods

hdme provides implementations of the following algorithms:

The methods implemented in the package include

-   Corrected Lasso for Linear Models (Loh and Wainwright (2012))
-   Corrected Lasso for Generalized Linear Models (Sorensen, Frigessi,
    and Thoresen (2015))
-   Matrix Uncertainty Selector for Linear Models (Rosenbaum and
    Tsybakov (2010))
-   Matrix Uncertainty Selector for Generalized Linear Models (Sorensen
    et al. (2018))
-   Matrix Uncertainty Lasso for Generalized Linear Models (Sorensen et
    al. (2018))
-   Generalized Dantzig Selector (James and Radchenko (2009))

## Contributions

Contributions to `hdme` are very welcome. If you have a question or
suspect you have found a bug, please [open an
Issue](https://github.com/osorensen/hdme/issues). Code contribution by
pull requests are also appreciated.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-james2009" class="csl-entry">

James, Gareth M., and Peter Radchenko. 2009. “A Generalized Dantzig
Selector with Shrinkage Tuning.” *Biometrika* 96 (2): 323–37.

</div>

<div id="ref-loh2012" class="csl-entry">

Loh, Po-Ling, and Martin J. Wainwright. 2012. “High-Dimensional
Regression with Noisy and Missing Data: Provable Guarantees with
Nonconvexity.” *Ann. Statist.* 40 (3): 1637–64.

</div>

<div id="ref-rosenbaum2010" class="csl-entry">

Rosenbaum, Mathieu, and Alexandre B. Tsybakov. 2010. “Sparse Recovery
Under Matrix Uncertainty.” *Ann. Statist.* 38 (5): 2620–51.

</div>

<div id="ref-sorensen2015" class="csl-entry">

Sorensen, Oystein, Arnoldo Frigessi, and Magne Thoresen. 2015.
“Measurement Error in Lasso: Impact and Likelihood Bias Correction.”
*Statistica Sinica* 25 (2): 809–29.

</div>

<div id="ref-sorensen2018" class="csl-entry">

Sorensen, Oystein, Kristoffer Herland Hellton, Arnoldo Frigessi, and
Magne Thoresen. 2018. “Covariate Selection in High-Dimensional
Generalized Linear Models with Measurement Error.” *Journal of
Computational and Graphical Statistics* 27 (4): 739–49.
<https://doi.org/10.1080/10618600.2018.1425626>.

</div>

</div>
