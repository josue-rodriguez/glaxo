
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glaxo

<!-- badges: start -->
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

This is a basic example which shows you how to solve a common problem:

``` r
library(glaxo)

Y <- subset(psych::bfi, select = 1:10)
Y <- na.omit(Y)

relaxed_glasso <- glaxo(Y[1:1000, ], progress = FALSE)
```

``` r
relaxed_glasso
#>         1      2      3      4      5      6      7      8      9     10
#> 1   0.000 -0.242 -0.068  0.000 -0.042  0.044  0.052  0.057  0.079  0.000
#> 2  -0.242  0.000  0.302  0.142  0.113  0.029 -0.002  0.144  0.000  0.000
#> 3  -0.068  0.302  0.000  0.163  0.317  0.000  0.000  0.000  0.000  0.000
#> 4   0.000  0.142  0.163  0.000  0.140 -0.046  0.181 -0.041  0.044 -0.142
#> 5  -0.042  0.113  0.317  0.140  0.000  0.065  0.000  0.000 -0.028 -0.039
#> 6   0.044  0.029  0.000 -0.046  0.065  0.000  0.257  0.129 -0.195  0.000
#> 7   0.052 -0.002  0.000  0.181  0.000  0.257  0.000  0.255 -0.195 -0.019
#> 8   0.057  0.144  0.000 -0.041  0.000  0.129  0.255  0.000 -0.072 -0.192
#> 9   0.079  0.000  0.000  0.044 -0.028 -0.195 -0.195 -0.072  0.000  0.369
#> 10  0.000  0.000  0.000 -0.142 -0.039  0.000 -0.019 -0.192  0.369  0.000
```

``` r
library(ggnetwork)
#> Loading required package: ggplot2

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

``` r
preds <- predict(relaxed_glasso, newdata = Y[1001:2000, ])
str(preds)
#> List of 2
#>  $ predictions: num [1:1000, 1:10] 1.87 2.1 2.81 3.03 3 ...
#>  $ beta_matrix: num [1:10, 1:10] 8.80e-01 -2.12e-01 -5.87e-02 7.86e-07 -3.83e-02 ...
round(head(preds$predictions), 3)
#>       [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10]
#> [1,] 1.865 5.430 5.217 5.095 5.113 3.944 3.562 3.644 3.336 4.007
#> [2,] 2.103 4.872 5.135 5.046 5.083 5.319 5.752 5.095 2.146 1.985
#> [3,] 2.808 4.047 4.267 4.179 4.142 3.602 3.645 3.687 3.313 5.086
#> [4,] 3.032 2.897 2.928 3.443 2.887 4.427 4.013 4.342 2.273 3.640
#> [5,] 3.003 4.330 4.200 3.718 3.721 5.130 5.180 4.220 2.753 2.189
#> [6,] 2.558 4.386 4.579 4.852 4.252 4.417 3.859 4.091 3.060 4.602
round(preds$beta_matrix, 3)
#>         [,1]   [,2]   [,3]   [,4]   [,5]   [,6]   [,7]   [,8]   [,9]  [,10]
#>  [1,]  0.880 -0.276 -0.078  0.000 -0.046  0.047  0.059  0.062  0.091  0.000
#>  [2,] -0.212  0.678  0.307  0.132  0.109  0.027 -0.002  0.138  0.000  0.000
#>  [3,] -0.059  0.298  0.658  0.150  0.300  0.000  0.000  0.000  0.000  0.000
#>  [4,]  0.000  0.152  0.177  0.781  0.144 -0.047  0.194 -0.042  0.048 -0.149
#>  [5,] -0.038  0.118  0.335  0.136  0.736  0.064  0.000  0.000 -0.029 -0.040
#>  [6,]  0.040  0.031  0.000 -0.045  0.066  0.759  0.273  0.132 -0.209  0.000
#>  [7,]  0.046 -0.002  0.000  0.168  0.000  0.242  0.674  0.245 -0.197 -0.018
#>  [8,]  0.051  0.149  0.000 -0.040  0.000  0.127  0.265  0.730 -0.075 -0.194
#>  [9,]  0.069  0.000  0.000  0.040 -0.027 -0.182 -0.193 -0.068  0.662  0.356
#> [10,]  0.000  0.000  0.000 -0.136 -0.039  0.000 -0.019 -0.189  0.382  0.711
```

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-meinshausen2007relaxed" class="csl-entry">

Meinshausen, Nicolai. 2007. “Relaxed Lasso.” *Computational Statistics &
Data Analysis* 52 (1): 374–93.

</div>

</div>
