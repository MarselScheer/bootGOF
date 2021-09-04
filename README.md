<!-- README.md is generated from README.Rmd. Please edit that file -->

[![R build
status](https://github.com/MarselScheer/bootGOF/workflows/R-CMD-check/badge.svg)](https://github.com/MarselScheer/bootGOF/actions)
[![Coverage
Status](https://img.shields.io/codecov/c/github/MarselScheer/bootGOF/develop.svg)](https://codecov.io/github/MarselScheer/bootGOF?branch=develop)
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
    #> R version 4.1.0 (2021-05-18)
    #> Platform: x86_64-pc-linux-gnu (64-bit)
    #> Running under: Ubuntu 20.04.2 LTS
    #> 
    #> Matrix products: default
    #> BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.8.so
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
    #> [1] bootGOF_0.1.0.9000
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] knitr_1.29        magrittr_1.5      pkgload_1.1.0     R6_2.4.1         
    #>  [5] rlang_0.4.10      fansi_0.4.1       stringr_1.4.0     tools_4.1.0      
    #>  [9] pkgbuild_1.0.8    checkmate_2.0.0   xfun_0.15         cli_2.0.2        
    #> [13] withr_2.4.1       htmltools_0.5.0   yaml_2.2.1        assertthat_0.2.1 
    #> [17] rprojroot_1.3-2   digest_0.6.25     crayon_1.3.4      processx_3.4.3   
    #> [21] callr_3.4.3       ps_1.3.3          testthat_2.3.2    glue_1.4.1       
    #> [25] evaluate_0.14     rmarkdown_2.3     stringi_1.4.6     compiler_4.1.0   
    #> [29] desc_1.2.0        backports_1.1.8   prettyunits_1.1.1 renv_0.14.0
