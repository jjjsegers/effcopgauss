effcopgauss
===========

This is a small R package to compute the efficient score function and efficient information matrix for semiparametric Gaussian copula models. It serves as a complement to the paper Segers, J., van den Akker, R., Werker, B. (2013) "Semiparametric Gaussian copula models: Geometry and efficient rank-based estimation", [arxiv:1306.6658](http://arxiv.org/abs/1306.6658).

Installation
------------

To install the package in R, you must first install the `devtools` package:

  > install.package("devtools")
  
Then, to install the `effcopgauss` package, simply do

  > library(devtools)
  
  > install_github("effcopgauss", "jjjsegers")

Usage
-----

If the package is installed, you can do for instance

  > library(effcopgauss)
  
  > ? effcopgauss
  
  > ? corrMatrix
  
  > ? effInfMat
  
to see documentation and a number of examples.