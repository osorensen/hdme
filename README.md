
<!-- README.md is generated from README.Rmd. Please edit that file -->
hdme
====

The goal of hdme is to provide penalized regression methods for High-Dimensional Measurement Error problems (errors-in-variables).

Installation
------------

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

`hdme` depends on `Rglpk`. In order to install `Rglpk` on macOS, you may need to first install `GLPK` by issuing on the command line.

``` bash
brew install glpk
```

Then install `Rglpk` using

``` r
install.packages("Rglpk")
```

Methods
-------

hdme provides implementations of the following algorithms:

The methods implemented in the package include

-   Corrected Lasso for Linear Models (Loh and Wainwright (2012))
-   Corrected Lasso for Generalized Linear Models (Sorensen, Frigessi, and Thoresen (2015))
-   Matrix Uncertainty Selector for Linear Models (Rosenbaum and Tsybakov (2010))
-   Matrix Uncertainty Selector for Generalized Linear Models (Sorensen et al. (2018))
-   Matrix Uncertainty Lasso for Generalized Linear Models (Sorensen et al. (2018))
-   Generalized Dantzig Selector (James and Radchenko (2009))

References
----------

James, Gareth M., and Peter Radchenko. 2009. “A Generalized Dantzig Selector with Shrinkage Tuning.” *Biometrika* 96 (2): 323–37.

Loh, Po-Ling, and Martin J. Wainwright. 2012. “High-Dimensional Regression with Noisy and Missing Data: Provable Guarantees with Nonconvexity.” *Ann. Statist.* 40 (3). The Institute of Mathematical Statistics: 1637–64.

Rosenbaum, Mathieu, and Alexandre B. Tsybakov. 2010. “Sparse Recovery Under Matrix Uncertainty.” *Ann. Statist.* 38 (5): 2620–51.

Sorensen, Oystein, Arnoldo Frigessi, and Magne Thoresen. 2015. “Measurement Error in Lasso: Impact and Likelihood Bias Correction.” *Statistica Sinica* 25 (2). Institute of Statistical Science, Academia Sinica: 809–29.

Sorensen, Oystein, Kristoffer Herland Hellton, Arnoldo Frigessi, and Magne Thoresen. 2018. “Covariate Selection in High-Dimensional Generalized Linear Models with Measurement Error.” *Journal of Computational and Graphical Statistics*. Taylor & Francis.
