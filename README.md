<!-- README.md is generated from README.Rmd. Please edit that file -->

[![R build
status](https://github.com/MarselScheer/bootGOF/workflows/R-CMD-check/badge.svg)](https://github.com/MarselScheer/bootGOF/actions)
[![Coverage
Status](https://img.shields.io/codecov/c/github/MarselScheer/bootGOF/develop.svg)](https://app.codecov.io/github/MarselScheer/bootGOF?branch=develop)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/bootGOF)](https://cran.r-project.org/package=bootGOF)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/bootGOF)](https://cran.r-project.org/package=bootGOF)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

# bootGOF

Bootstrap based goodness-of-fit tests for (linear) models. Assume you
have fitted a statistical model, e.g. classical linear model or
generalized linear model or a model that follows
*Y* = *m*(*β*<sup>⊤</sup>*X*) + *ϵ*. This package allows to perform a
rigorous statistical test to check if the chosen model family is
correct.

## Example

First we generate a data-set in order to apply the package.

    set.seed(1)
    N <- 100
    X1 <- rnorm(N)
    X2 <- rnorm(N)
    d <- data.frame(
      y = rpois(n = N, lambda = exp(4 + X1 * 2 + X2 * 6)),
      x1 = X1,
      x2 = X2)

Note that both covariates influence the dependent variable *Y*. Taking
only one of the covariates into account obviously leads to a model
family that is not correct and the GOF-test should reveal that:

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

On the other hand assuming the correct model family should in general
not be rejected by the GOF-test:

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

…

## Installation

You can install it from CRAN

    install.packages("bootGOF")

or github

    devtools::install_github("MarselScheer/bootGOF")

## Tests

After installing the package you can execute the unit tests of the
package in your environment by calling:

    library(tinytest)
    tinytest::test_package("bootGOF")

# sessionInfo

    sessionInfo()
    #> R Under development (unstable) (2025-08-19 r88650)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Ubuntu 24.04.2 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    #> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    #>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    #>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    #>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    #>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    #> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    #> 
    #> time zone: Etc/UTC
    #> tzcode source: system (glibc)
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices datasets  utils     methods   base     
    #> 
    #> other attached packages:
    #> [1] bootGOF_0.1.1
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] digest_0.6.37     desc_1.4.3        backports_1.5.0   R6_2.6.1         
    #>  [5] fastmap_1.2.0     xfun_0.53         knitr_1.50        htmltools_0.5.8.1
    #>  [9] rmarkdown_2.29    cli_3.6.5         renv_1.1.5        withr_3.0.2      
    #> [13] pkgload_1.4.0     compiler_4.6.0    rprojroot_2.1.0   tools_4.6.0      
    #> [17] pkgbuild_1.4.8    checkmate_2.3.3   evaluate_1.0.4    yaml_2.3.10      
    #> [21] rlang_1.1.6
