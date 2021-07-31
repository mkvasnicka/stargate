
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stargate

<!-- badges: start -->
<!-- badges: end -->

The goal of **stargate** package is to simplify production of formatted
tables that hold regression analysis results from several models
side-by-side.

For many years, we have used **stargazer** package for this purpose.
**stargazer** is great and every applied econometrician has to be
thankful to Marek Hlaváč for kindly providing it. However, this package
is implemented as one monolithic function, which has two drawbacks:

1.  It is very difficult for an user to implement a method for an
    estimated object not covered in **stargazer**.
2.  It has too many complex parameters. A set of lightweight
    manipulation function joined with pipes (in the tidyverse style)
    seems preferable.

This package is an attempt to rewrite **stargazer** in this fashion. To
pay homage to the original package, it is named similarly: **stargate**.

## Installation

<!--
You can install the released version of stargate from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("stargate")
```
-->

At the present time, there is only the development version. It is
available from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mkvasnicka/stargate")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(stargate)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
