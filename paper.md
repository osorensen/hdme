---
title: 'hdme: High-Dimensional Regression with Measurement Error'
tags:
  - R
  - regression
  - variable selection
  - measurement error
authors:
  - name: Oystein Sorensen
    orcid: 0000-0003-0724-3542
    affiliation: 1 # (Multiple affiliations must be quoted)
affiliations:
 - name: Center for Lifespan Changes in Brain and Cognition, Department of Psychology, University of Oslo
   index: 1
date: 12 April 2019
bibliography: paper.bib
---

# Summary

Many problems in science involve using measured variables to explain an outcome of interest using some statistical regression model. In high-dimensional problems, characterized by having a very large number of variables, one often focuses on finding a subset of variables with good explanatory power. An example from cancer research involves finding gene expressions or other biomarkers which can explain disease progression, from a large set of candidates [@kristensen2014]. Another example is customer analytics, where it may be of interest to find out which variables predict whether customers will return or not, and variables of interest include factors like previous purchasing patterns, demographics, and satisfaction measures [@baesens2014]. 

The lasso [@tibshirani1996] and the Dantzig selector [@candes2007;@james2009] are popular methods for variable selection in this type of problems, combining computational speed with good statistical properties [@buhlmann2011]. In many practical applications, the process of measuring the variables of interest is subject to measurement error [@carroll2006], but this additional source of noise is neglected by the aforementioned models. Such measurement error has been shown to lead to worse variable selection properties of the lasso [@sorensen2015], typically involving an increased number of false positive selections. A corrected lasso has been proposed and analyzed by @loh2012 for linear models and @sorensen2015 for generalized linear models. It has been applied by @vasquez2019 in a problem involving measurement of serum biomarkers. For the Dantzig selector, @rosenbaum2010 proposed the Matrix Uncertainty Selector (MUS) for linear models, which was extended to the generalized linear model case by @sorensen2018 with an algorithm named GMUS (Generalized MUS).

``hdme`` is an R [@rbib] package containing implementations of both the corrected lasso and the MU selector for high-dimensional measurement error problems. Its main functions are ``gmus()`` and ``corrected_lasso()``. Additional functions provide opportunities for hyperparameter tuning using cross-validation or the elbow rule [@rosenbaum2010], and plotting tools for visualizing the model fit. The underlying numerical procedures are implemented in ``C++`` using the ``RcppArmadillo`` package [@eddelbuettel2014] and linear programming with ``Rglpk`` [@theussl2019]. ``hdme`` is available from the comprehensive R archive network (CRAN) at https://CRAN.R-project.org, and the latest development version is available at https://github.com/osorensen/hdme. The package vignette, which can be opened in ``R`` with the command ``vignette("hdme")``, contains a step-by-step introduction to the models implemented in the package.


# Acknowledgements

The author would like to thank Arnoldo Frigessi, Kristoffer Herland Hellton, and Magne Thoresen for helpful discussions while developing the package.

# References
