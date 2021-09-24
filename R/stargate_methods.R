# -------------------------------------
# Script:   stargate_methods.R
# Author:   Michal Kvasnička
# Purpose:  This script includes sg_model() methods for selected estimators.
# Inputs:   none
# Outputs:  none
# Notes:    none
#
# Copyright(c) Corporation Name
# -------------------------------------

#' Convert an estimated lm model to a stargate model
#'
#' @param m A estimated lm model.
#' @param name The model's name in the final model table.
#' @param vcov The vcov function used by \code{lmtest::coeftest()}.
#' @param ... Other parameters sent to the \code{lmtest::coeftest()}.
#' @return The sg model representation of the estimate.
#' @family stargate model
#' @examples
#' library(tibble)
#' library(sandwich)
#' n <- 1e3
#' df <- tibble(
#'     x1 = rnorm(n),
#'     x2 = rnorm(n),
#'     x3 = rnorm(n),
#'     z = sample(1:10, size = n, replace = TRUE),
#'     e  = rnorm(n),
#'     y  = x1 - 2 * x2 + 3 * x3 + e
#' )
#' m1 <- lm(y ~ x1 + x2, df)
#' sm1 <- sg_model(m1)
#' sm2 <- sg_model(m1, vcov = vcovCL, cluster = ~z)
#' @export
sg_model.lm <- function(m, name = "", vcov = NULL, ...) {
    if (inherits(m, "stargate"))
        return(m)
    if (!requireNamespace("lmtest", quietly = TRUE)) {
        stop("Package \"lmtest\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    if (is.null(vcov)) {
        coefs <- broom::tidy(m)
    } else {
        coefs <- broom::tidy(lmtest::coeftest(m, vcov. = vcov, ...))
    }
    stats <- broom::glance(m) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), list)) |>
        tidyr::pivot_longer(dplyr::everything(),
                            names_to = "stat", values_to = "val")
    name <- tibble::tibble(name = name)
    structure(list(coefs = coefs, stats = stats, name = name),
              class = c("sg_model", "stargate"))
}
