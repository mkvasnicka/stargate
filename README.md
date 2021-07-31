
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

However, this package is just in a rudimentary state—it is more a proof
of the concept and a playground than a real thing. It is here to raise
some feedback, comments, and wishes. If used, it must be used with
extreme care:

-   It can handle only the estimates handled by **broom** at the present
    time.
-   Anything can change at any time.
-   Anything can fail you expectations at any time.

You have been warned. Enjoy!

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

This is a basic example which shows you how to use **stargate**. Load
the necessary packages first:

``` r
library(stargate)
library(tibble)
library(kableExtra)
```

Estimates some models (simulated here):

``` r
n <- 1e3
df <- tibble(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    e  = rnorm(n),
    y  = x1 - 2 * x2 + 3 * x3 + e
)

m1 <- lm(y ~ x1 + x2, df)
m2 <- lm(y ~ x1 + x2 + x3, df)
m3 <- lm(y ~ x1 + x2 * x3, df)
```

Convert the estimates to their **stargate** representation: one model
with `sg_model()`, many models with `sg_table()`:

``` r
sm1 <- sg_model(m1)
stab <- sg_table(sm1, m2, m3)
#> Warning: Unknown or uninitialised column: `model_id`.

#> Warning: Unknown or uninitialised column: `model_id`.

#> Warning: Unknown or uninitialised column: `model_id`.
```

Transform the model table to your liking:

``` r
stab <- stab |>
    sg_remove(coefs = "Interc", regex = TRUE) |> 
    sg_remove(stats = c("r.squared", "logLik", "nobs"), keep = TRUE)
```

Format the table:

``` r
sftab <- stab |> 
    sg_format()
```

Print the table:

``` r
kbl(sftab, format = "pipe") |> 
    kable_classic() |> 
    row_spec(5, hline_after = TRUE)
#> Warning in kable_styling(kable_input, "none", htmltable_class = light_class, :
#> Please specify format in kable. kableExtra can customize either HTML or LaTeX
#> outputs. See https://haozhu233.github.io/kableExtra/ for details.
#> Warning in row_spec(kable_classic(kbl(sftab, format = "pipe")), 5, hline_after
#> = TRUE): Please specify format in kable. kableExtra can customize either HTML or
#> LaTeX outputs. See https://haozhu233.github.io/kableExtra/ for details.
```

| oo        | \(1\)              | \(2\)              | \(3\)              |
|:----------|:-------------------|:-------------------|:-------------------|
| x1        | 0.99\*\*\* (0.10)  | 1.00\*\*\* (0.03)  | 1.00\*\*\* (0.03)  |
| x2        | -2.02\*\*\* (0.10) | -1.99\*\*\* (0.03) | -1.99\*\*\* (0.03) |
| x3        |                    | 3.02\*\*\* (0.03)  | 3.02\*\*\* (0.03)  |
| x2:x3     |                    |                    | 0.00 (0.03)        |
| r.squared | 0.35               | 0.93               | 0.93               |
| logLik    | -2573.77           | -1455.41           | -1455.41           |
| nobs      | 1000               | 1000               | 1000               |

Or do all that at once:

``` r
sg_table(sm1, m2, m3) |> 
    sg_remove(coefs = "Interc", regex = TRUE) |> 
    sg_remove(stats = c("r.squared", "logLik", "nobs"), keep = TRUE) |> 
    sg_format() |> 
    kbl(format = "pipe") |> 
    kable_classic() |> 
    row_spec(5, hline_after = TRUE)
#> Warning: Unknown or uninitialised column: `model_id`.

#> Warning: Unknown or uninitialised column: `model_id`.

#> Warning: Unknown or uninitialised column: `model_id`.
#> Warning in kable_styling(kable_input, "none", htmltable_class = light_class, :
#> Please specify format in kable. kableExtra can customize either HTML or LaTeX
#> outputs. See https://haozhu233.github.io/kableExtra/ for details.
#> Warning in
#> row_spec(kable_classic(kbl(sg_format(sg_remove(sg_remove(sg_table(sm1, : Please
#> specify format in kable. kableExtra can customize either HTML or LaTeX outputs.
#> See https://haozhu233.github.io/kableExtra/ for details.
```

| oo        | \(1\)              | \(2\)              | \(3\)              |
|:----------|:-------------------|:-------------------|:-------------------|
| x1        | 0.99\*\*\* (0.10)  | 1.00\*\*\* (0.03)  | 1.00\*\*\* (0.03)  |
| x2        | -2.02\*\*\* (0.10) | -1.99\*\*\* (0.03) | -1.99\*\*\* (0.03) |
| x3        |                    | 3.02\*\*\* (0.03)  | 3.02\*\*\* (0.03)  |
| x2:x3     |                    |                    | 0.00 (0.03)        |
| r.squared | 0.35               | 0.93               | 0.93               |
| logLik    | -2573.77           | -1455.41           | -1455.41           |
| nobs      | 1000               | 1000               | 1000               |
