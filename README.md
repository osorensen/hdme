
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hdme

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/hdme)](https://cran.r-project.org/package=hdme)
[![Build
Status](https://travis-ci.org/osorensen/hdme.svg?branch=master)](https://travis-ci.org/osorensen/hdme)

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
devtools::install_github("osorensen/hdme")
```

### Note when installing on macOS

The package `Rglpk` is suggested when installing `hdme`. In order to
install `Rglpk` on macOS, you may need to first install `GLPK` by
issuing the following statement on the command line:

``` bash
brew install glpk
```

Then install `Rglpk`:

``` r
install.packages("Rglpk")
```

If you are not able to install `Rglpk`, then please install the
suggested package `lpSolveAPI` instead, using the command

``` r
install.packages("lpSolveAPI")
```

The functions in `hdme` that use `Rglpk`, will switch to `lpSolveAPI`
automatically if the former is not available.

## Methods

hdme provides implementations of the following algorithms:

The methods implemented in the package include

  - Corrected Lasso for Linear Models (Loh and Wainwright (2012))
  - Corrected Lasso for Generalized Linear Models (Sorensen, Frigessi,
    and Thoresen (2015))
  - Matrix Uncertainty Selector for Linear Models (Rosenbaum and
    Tsybakov (2010))
  - Matrix Uncertainty Selector for Generalized Linear Models (Sorensen
    et al. (2018))
  - Matrix Uncertainty Lasso for Generalized Linear Models (Sorensen et
    al. (2018))
  - Generalized Dantzig Selector (James and Radchenko (2009))

## References

<div id="refs" class="references">

<div id="ref-james2009">

James, Gareth M., and Peter Radchenko. 2009. “A Generalized Dantzig
Selector with Shrinkage Tuning.” *Biometrika* 96 (2): 323–37.

</div>

<div id="ref-loh2012">

Loh, Po-Ling, and Martin J. Wainwright. 2012. “High-Dimensional
Regression with Noisy and Missing Data: Provable Guarantees with
Nonconvexity.” *Ann. Statist.* 40 (3). The Institute of Mathematical
Statistics: 1637–64.

</div>

<div id="ref-rosenbaum2010">

Rosenbaum, Mathieu, and Alexandre B. Tsybakov. 2010. “Sparse Recovery
Under Matrix Uncertainty.” *Ann. Statist.* 38 (5): 2620–51.

</div>

<div id="ref-sorensen2015">

Sorensen, Oystein, Arnoldo Frigessi, and Magne Thoresen. 2015.
“Measurement Error in Lasso: Impact and Likelihood Bias Correction.”
*Statistica Sinica* 25 (2). Institute of Statistical Science, Academia
Sinica: 809–29.

</div>

<div id="ref-sorensen2018">

Sorensen, Oystein, Kristoffer Herland Hellton, Arnoldo Frigessi, and
Magne Thoresen. 2018. “Covariate Selection in High-Dimensional
Generalized Linear Models with Measurement Error.” *Journal of
Computational and Graphical Statistics*. Taylor & Francis.

</div>

</div>
