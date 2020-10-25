
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://travis-ci.org/MarselScheer/GOF.svg?branch=master)](https://travis-ci.org/MarselScheer/GOF)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN\_Status\_Badge\_version\_ago](https://www.r-pkg.org/badges/version-ago/GOF)](https://cran.r-project.org/package=GOF)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/GOF)](https://cran.r-project.org/package=GOF)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

# GOF

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
install.packages("GOF")
```

or github

``` r
devtools::install_github("MarselScheer/GOF")
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
#> [1] GOF_0.0.0.9000    badgecreatr_0.2.0 testthat_2.3.2   
#> 
#> loaded via a namespace (and not attached):
#>  [1] xfun_0.15         remotes_2.2.0     purrr_0.3.4       rematch2_2.1.2   
#>  [5] vctrs_0.3.2       usethis_1.6.1     htmltools_0.5.0   yaml_2.2.1       
#>  [9] rlang_0.4.7       pkgbuild_1.0.8    pkgdown_1.5.1     pillar_1.4.6     
#> [13] glue_1.4.1        withr_2.2.0       mockery_0.4.2     sessioninfo_1.1.1
#> [17] lifecycle_0.2.0   stringr_1.4.0     devtools_2.3.0    memoise_1.1.0    
#> [21] evaluate_0.14     knitr_1.29        callr_3.4.3       ps_1.3.3         
#> [25] parallel_4.0.0    fansi_0.4.1       Rcpp_1.0.4.6      renv_0.11.0      
#> [29] backports_1.1.8   checkmate_2.0.0   desc_1.2.0        pkgload_1.1.0    
#> [33] fs_1.4.2          digest_0.6.25     stringi_1.4.6     processx_3.4.3   
#> [37] tinytest_1.2.1    rprojroot_1.3-2   cli_2.0.2         tools_4.0.0      
#> [41] magrittr_1.5      tibble_3.0.3      crayon_1.3.4      pkgconfig_2.0.3  
#> [45] MASS_7.3-51.6     ellipsis_0.3.1    xml2_1.3.2        prettyunits_1.1.1
#> [49] assertthat_0.2.1  rmarkdown_2.3     roxygen2_7.1.0    R6_2.4.1         
#> [53] compiler_4.0.0    git2r_0.27.1
```
