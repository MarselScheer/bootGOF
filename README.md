
<!-- README.md is generated from README.Rmd. Please edit that file -->

    #> Warning: package 'badgecreatr' was built under R version 4.0.2

[![Build
Status](https://travis-ci.org/MarselScheer/bootGOF.svg?branch=master)](https://travis-ci.org/MarselScheer/bootGOF)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN\_Status\_Badge\_version\_ago](https://www.r-pkg.org/badges/version-ago/bootGOF)](https://cran.r-project.org/package=bootGOF)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/bootGOF)](https://cran.r-project.org/package=bootGOF)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

# bootGOF

Bootstrap based goodness-of-fit tests for (linear) models. Assume you
have fitted a statistical model, e.g. classical linear model or
generalized linear model or a model that follows
\(Y = m(\beta^\top X) + \epsilon\). This package allows to perform a
rigorous statistical test to check if the chosen model family is
correct.

## Example

First we generate a data-set in order to apply the package.

``` r
set.seed(1)
N <- 100
X1 <- rnorm(N)
X2 <- rnorm(N)
d <- data.frame(
  y = rpois(n = N, lambda = exp(4 + X1 * 2 + X2 * 6)),
  x1 = X1,
  x2 = X2)
```

Note that both covariates influence the dependent variable \(Y\). Taking
only one of the covariates into account obviously leads to a model
family that is not correct and the GOF-test should reveal that:

``` r
fit <- glm(y ~ x1, data = d, family = poisson())

library(bootGOF)
mt <- GOF_model(
  model = fit,
  data = d,
  nmb_boot_samples = 100,
  simulator_type = "parametric",
  y_name = "y",
  Rn1_statistic = Rn1_KS$new())
mt$get_pvalue()
#> [1] 0
```

On the other hand assuming the correct model family should in general
not be rejected by the GOF-test:

``` r
fit <- glm(y ~ x1 + x2, data = d, family = poisson())
mt <- GOF_model(
  model = fit,
  data = d,
  nmb_boot_samples = 100,
  simulator_type = "parametric",
  y_name = "y",
  Rn1_statistic = Rn1_KS$new())
mt$get_pvalue()
#> [1] 0.61
```

…

## Installation

You can install it from CRAN

``` r
install.packages("bootGOF")
```

or github

``` r
devtools::install_github("MarselScheer/bootGOF")
```

# sessionInfo

``` r
sessionInfo()
#> R version 4.0.0 (2020-04-24)
#> Platform: x86_64-pc-linux-gnu (64-bit)
#> Running under: Ubuntu 20.04 LTS
#> 
#> Matrix products: default
#> BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/openblas-openmp/libopenblasp-r0.3.8.so
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
#>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=C             
#>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices datasets  utils     methods   base     
#> 
#> other attached packages:
#> [1] bootGOF_0.0.0.9000 badgecreatr_0.2.0 
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.25   R6_2.4.1        backports_1.1.8 git2r_0.27.1   
#>  [5] magrittr_1.5    evaluate_0.14   rlang_0.4.7     stringi_1.4.6  
#>  [9] renv_0.11.0     checkmate_2.0.0 rmarkdown_2.3   tools_4.0.0    
#> [13] stringr_1.4.0   xfun_0.15       yaml_2.2.1      compiler_4.0.0 
#> [17] htmltools_0.5.0 knitr_1.29
```
