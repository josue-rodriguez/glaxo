
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glaxo

<!-- badges: start -->

<img src='inst/glaxo-logo.png' align="center" height="240" />

[![CircleCI build
status](https://circleci.com/gh/josue-rodriguez/glaxo.svg?style=shield)](https://circleci.com/gh/josue-rodriguez/glaxo)
<!-- badges: end -->

The glaxo package provides an implementation of the relaxed lasso for
Gaussian graphical models based on the simple algorithm described in
section 2.2 of Meinshausen (2007). Because the classical lasso
introduces bias into the parameters, the motivation behind the relaxed
lasso is to *debias* the estimates.

## Installation

You can install the released version of glaxo from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("glaxo")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("josue-rodriguez/glaxo")
```

## Example

The main function in this package is called `glaxo` (relaxed + glasso).
In it’s most basic form, it takes a `data.frame` or `matrix` and
estimates the relaxed lasso solution for the graphical model

``` r
library(glaxo)

Y <- psych::bfi[, 1:10]
Y <- na.omit(Y)

relaxed_glasso <- glaxo(Y, progress = FALSE)
```

``` r
relaxed_glasso
#>         1      2      3      4      5      6      7      8      9     10
#> 1   0.000 -0.241 -0.104 -0.011 -0.014  0.046  0.065  0.036  0.111 -0.019
#> 2  -0.241  0.000  0.284  0.156  0.146  0.003  0.007  0.114 -0.007  0.024
#> 3  -0.104  0.284  0.000  0.163  0.350  0.004  0.022  0.000  0.000 -0.012
#> 4  -0.011  0.156  0.163  0.000  0.111 -0.036  0.140 -0.021  0.008 -0.141
#> 5  -0.014  0.146  0.350  0.111  0.000  0.055 -0.014  0.010  0.000 -0.050
#> 6   0.046  0.003  0.004 -0.036  0.055  0.000  0.297  0.126 -0.155 -0.039
#> 7   0.065  0.007  0.022  0.140 -0.014  0.297  0.000  0.180 -0.184 -0.047
#> 8   0.036  0.114  0.000 -0.021  0.010  0.126  0.180  0.000 -0.124 -0.172
#> 9   0.111 -0.007  0.000  0.008  0.000 -0.155 -0.184 -0.124  0.000  0.351
#> 10 -0.019  0.024 -0.012 -0.141 -0.050 -0.039 -0.047 -0.172  0.351  0.000
```

The resulting object can also be plotted using standard `ggplot2` syntax

``` r
library(ggnetwork)

plot_edges(relaxed_glasso, layout = "circle") +
  scale_size(range = c(0,2)) +
  guides(size = FALSE) +
  scale_color_manual(values = c("#FAA43A", "#5DA5DA"),
                     name = "",
                     labels = c("Negative", "Positive")) +
  theme_facet() +
  theme(legend.position = "top")
```

<img src="man/figures/README-plot-1.png" width="100%" />

Predictions can be made on new data using the relationship between
Gaussian graphical models and linear regression (e.g., Stevens 1998)

``` r
preds <- predict(relaxed_glasso, newdata = Y[1001:2000, ])
str(preds)
#> List of 2
#>  $ predictions: num [1:1000, 1:10] 1.83 2 2.95 3.14 2.98 ...
#>  $ beta_matrix: num [1:10, 1:10] 0.8624 -0.2127 -0.0885 -0.0107 -0.0128 ...
round(head(preds$predictions), 3)
#>       [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10]
#> [1,] 1.833 5.467 5.198 5.118 5.231 3.902 3.521 3.675 3.207 4.139
#> [2,] 2.004 4.814 5.064 5.121 5.116 5.261 5.695 5.077 2.170 1.955
#> [3,] 2.952 4.094 4.224 4.068 4.180 3.626 3.877 3.672 3.538 4.879
#> [4,] 3.135 2.768 2.832 3.573 2.896 4.425 4.067 4.354 2.319 3.781
#> [5,] 2.978 4.330 4.216 3.791 3.531 5.048 4.947 4.305 2.639 2.264
#> [6,] 2.649 4.375 4.537 4.731 4.462 4.410 3.920 3.934 3.218 4.482
round(preds$beta_matrix, 3)
#>         [,1]   [,2]   [,3]   [,4]   [,5]   [,6]   [,7]   [,8]   [,9]  [,10]
#>  [1,]  0.862 -0.274 -0.122 -0.012 -0.016  0.049  0.073  0.039  0.126 -0.021
#>  [2,] -0.213  0.670  0.293  0.144  0.141  0.003  0.007  0.106 -0.007  0.024
#>  [3,] -0.088  0.274  0.627  0.146  0.329  0.004  0.021  0.000  0.000 -0.011
#>  [4,] -0.011  0.169  0.182  0.785  0.116 -0.036  0.148 -0.022  0.009 -0.147
#>  [5,] -0.013  0.150  0.373  0.105  0.712  0.053 -0.014  0.010  0.000 -0.050
#>  [6,]  0.043  0.003  0.005 -0.035  0.057  0.761  0.309  0.125 -0.165 -0.040
#>  [7,]  0.059  0.008  0.023  0.132 -0.014  0.285  0.700  0.172 -0.188 -0.047
#>  [8,]  0.034  0.122  0.000 -0.021  0.010  0.127  0.189  0.770 -0.132 -0.178
#>  [9,]  0.098 -0.007  0.000  0.007  0.000 -0.145 -0.180 -0.116  0.672  0.341
#> [10,] -0.018  0.025 -0.013 -0.134 -0.050 -0.038 -0.048 -0.165  0.362  0.714
```

# Relaxed lasso vs regular lasso

The intuition behind the relaxed lasso is that the lasso algorithm is
run on the data once for an initial screening and then again to screen
out false positives. This can result in better performance than the
traditional lasso solution

``` r
library(GGMncv)

main <- GGMnonreg::gen_net()
n <- 5000
Y <- MASS::mvrnorm(n, mu = rep(0, 20), main$cors)

regular_glasso <- GGMncv::ggmncv(cor(Y), n = nrow(Y), 
                                 penalty = "lasso", 
                                 ic = "bic",
                                 progress = FALSE)

relaxed_glasso <- glaxo(Y, progress = FALSE, ic = "bic")


glaxo:::performance(Estimate = regular_glasso$adj, True = main$adj)
#>       measure     score
#> 1 Specificity 0.5413534
#> 2 Sensitivity 1.0000000
#> 3   Precision 0.4830508
#> 4      Recall 1.0000000
#> 5    F1_score 0.6514286
#> 6         MCC 0.5113719
glaxo:::performance(Estimate = relaxed_glasso$adj, True = main$adj)
#>       measure     score
#> 1 Specificity 0.3834586
#> 2 Sensitivity 1.0000000
#> 3   Precision 0.4100719
#> 4      Recall 1.0000000
#> 5    F1_score 0.5816327
#> 6         MCC 0.3965421
```

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-meinshausen2007relaxed" class="csl-entry">

Meinshausen, Nicolai. 2007. “Relaxed Lasso.” *Computational Statistics &
Data Analysis* 52 (1): 374–93.

</div>

<div id="ref-stevens1998inverse" class="csl-entry">

Stevens, Guy VG. 1998. “On the Inverse of the Covariance Matrix in
Portfolio Analysis.” *The Journal of Finance* 53 (5): 1821–27.

</div>

</div>
