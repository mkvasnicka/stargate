
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
3.  It has its own output filters to LaTeX, HTML, etc., and hence it is
    impossible to customize output for your own liking.

This package is an attempt to rewrite **stargazer** remove these
obstacles. To pay homage to the original package, it is named similarly:
**stargate**.

1.  Every estimated object class should have its own method to convert
    the estimate into a standard format. The broom is used now but
    dedicated methods would be provided later to customize the process,
    e.g. to include robust SEs.
2.  The workflow must be based on simples function that send their
    outputs between themselves through pipes.
3.  The final output must be a *tibble* (convertible to *data.frame*,
    etc.), and hence everyone can create the final table with any tool
    that suits him or her, e.g. `kbl()` from **kableExtra**.

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

You can also convert at least one estimate to a stargate model or table
and then add other using the plus (`+`) sign:

``` r
stab <- sm1 + m2 + m3
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

Rename coefficients and statistics as you like:

``` r
stab <- stab |> 
    sg_rename(coefs = c("a" = "x", " x " = ":"), regex = TRUE) |> 
    sg_rename(stats = c("R2" = "r.squared", "log Lik." = "logLik"))
```

Format the table:

``` r
sftab <- stab |> 
    sg_format()
```

Present:

``` r
sftab <- sftab |> 
    sg_present()
```

Print the table:

``` r
kbl(sftab, format = "pipe") |> 
    kable_classic()
#> Warning in kable_styling(kable_input, "none", htmltable_class = light_class, :
#> Please specify format in kable. kableExtra can customize either HTML or LaTeX
#> outputs. See https://haozhu233.github.io/kableExtra/ for details.
```

|          | \(1\)              | \(2\)              | \(3\)              |
|:---------|:-------------------|:-------------------|:-------------------|
| a1       | 1.03\*\*\* (0.10)  | 1.01\*\*\* (0.03)  | 1.01\*\*\* (0.03)  |
| a2       | -2.05\*\*\* (0.10) | -2.02\*\*\* (0.03) | -2.02\*\*\* (0.03) |
| a3       |                    | 3.03\*\*\* (0.03)  | 3.03\*\*\* (0.03)  |
| a2 x a3  |                    |                    | 0.02 (0.03)        |
| R2       | 0.33               | 0.94               | 0.94               |
| log Lik. | -2583.09           | -1399.94           | -1399.68           |
| nobs     | 1000               | 1000               | 1000               |

Or do all that at once:

``` r
sg_table(sm1, m2, m3) |> 
    sg_remove(coefs = "Interc", regex = TRUE) |> 
    sg_remove(stats = c("r.squared", "logLik", "nobs"), keep = TRUE) |> 
    sg_rename(coefs = c("a" = "x", " x " = ":"), regex = TRUE) |> 
    sg_rename(stats = c("R2" = "r.squared", "log Lik." = "logLik",
                        "num. of. obs." = "nobs")) |> 
    sg_format(style = c("{estimate}", "({std.error})"),
              format = list(nsmall = 3)) |> 
    sg_present() |> 
    kbl(format = "pipe") |> 
    kable_classic()
#> Warning: Unknown or uninitialised column: `model_id`.

#> Warning: Unknown or uninitialised column: `model_id`.

#> Warning: Unknown or uninitialised column: `model_id`.
#> Warning in kable_styling(kable_input, "none", htmltable_class = light_class, :
#> Please specify format in kable. kableExtra can customize either HTML or LaTeX
#> outputs. See https://haozhu233.github.io/kableExtra/ for details.
```

|               | \(1\)    | \(2\)    | \(3\)    |
|:--------------|:---------|:---------|:---------|
| a1            | 1.03     | 1.01     | 1.01     |
|               | (0.10)   | (0.03)   | (0.03)   |
| a2            | -2.05    | -2.02    | -2.02    |
|               | (0.10)   | (0.03)   | (0.03)   |
| a3            |          | 3.03     | 3.03     |
|               |          | (0.03)   | (0.03)   |
| a2 x a3       |          |          | 0.02     |
|               |          |          | (0.03)   |
| R2            | 0.33     | 0.94     | 0.94     |
| log Lik.      | -2583.09 | -1399.94 | -1399.68 |
| num. of. obs. | 1000     | 1000     | 1000     |

## What next

-   Implement the stuff. :-)
-   Provide methods for estimation objects.
-   Write a vignette how to write a new method.
-   Provide some auto-setup for the function to simplify the process.
-   Tune the output to LaTeX, HTML, Markdown, and possibly
    Word/LibreOffice.
