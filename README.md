# hdme
R-package containing penalized regression methods for High-Dimensional Measurement Error problems (errors-in-variables)

The methods implemented in the package include
* Corrected Lasso for Linear Models [Loh and Wainwright 2012](https://projecteuclid.org/euclid.aos/1346850068) 
* Corrected Lasso for Generalized Linear Models [Sørensen, Frigessi, and Thoresen 2015](http://www3.stat.sinica.edu.tw/statistica/j25n2/j25n220/j25n220.html)
* Matrix Uncertainty Selector for Linear Models [Rosenbaum and Tsybakov 2010](https://projecteuclid.org/euclid.aos/1278861455)
* Matrix Uncertainty Selector for Generalized Linear Models [Sørensen, Hellton, Frigessi, and Thoresen 2016](http://www.tandfonline.com/doi/full/10.1080/10618600.2018.1425626)
* Matrix Uncertainty Lasso for Generalized Linear Models [Sørensen, Hellton, Frigessi, and Thoresen 2016](https://arxiv.org/abs/1407.1070)
* Generalized Dantzig Selector [James and Radchenko 2009](http://biomet.oxfordjournals.org/content/96/2/323.full.pdf?keytype=ref&ijkey=fqYKS2eOTNpmWmd)


To install this package in R:
> install.packages("devtools") # Run this line if you do not have the devtools package installed already

> devtools::install_github("osorensen/hdme")

> library(hdme) # Load the package
