# -------------------------------------
# Script:
# Author:
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Corporation Name
# -------------------------------------

# library(tibble)
# library(dplyr)
# library(broom)
# library(tidyr)
# library(purrr)
# library(glue)
# library(stringr)


#' Convert an estimated model to a stargate model
#'
#' @param m A estimated model.
#' @param name The model's name in the final model table.
#' @param ... Other parameters sent to the \code{sg_model} methods.
#' @return The sg model representation of the estimate.
#' @examples
#' n <- 1e3
#' df <- tibble(
#'     x1 = rnorm(n),
#'     x2 = rnorm(n),
#'     x3 = rnorm(n),
#'     e  = rnorm(n),
#'     y  = x1 - 2 * x2 + 3 * x3 + e
#' )
#' m1 <- lm(y ~ x1 + x2, df)
#' sm1 <- sg_model(m1)
#' @export
sg_model <- function(m, name, ...)
    UseMethod("sg_model")

#' @export
sg_model.default <- function(m, name = "") {
    if (inherits(m, "stargate"))
        return(m)
    coefs <- broom::tidy(m)
    stats <- broom::glance(m) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), list)) |>
        tidyr::pivot_longer(dplyr::everything(),
                            names_to = "stat", values_to = "val")
    name <- tibble::tibble(name = name)
    structure(list(coefs = coefs, stats = stats, name = name),
              class = c("sg_model", "stargate"))
}


#' @export
sg_table <- function(...) {
    count_models_in_tables <- function(x) {
        count <- purrr::map_int(x,
                                ~ dplyr::case_when(
                                    inherits(., "sg_model") ~ 0L,
                                    inherits(., "sg_table") ~ length(unique(.$name$model_id)),
                                    TRUE ~ -1L
                                ))
        if (any(count == -1L)) stop("OOOOOoooo!")
        sum(count)
    }
    add_model_id <- function(x, var, id) {
        {tab <- x[[var]]
        if (inherits(x, "sg_model"))
            tab <- tab |>
                dplyr::mutate(model_id = id) |>
                dplyr::select(model_id, dplyr::everything())
        tab}
    }
    mods <- list(...)
    mods <- purrr::map(mods, sg_model)
    first_model_id <- count_models_in_tables(mods)
    model_ids <- first_model_id + mods |>
        purrr::map_int(~class(.)[1] == "sg_model") |>
        cumsum()
    coefs <- purrr::map2(mods, model_ids, ~add_model_id(.x, "coefs", .y)) |>
        dplyr::bind_rows()
    stats <- purrr::map2(mods, model_ids, ~add_model_id(.x, "stats", .y)) |>
        dplyr::bind_rows()
    name <- purrr::map2(mods, model_ids, ~add_model_id(.x, "name", .y)) |>
        dplyr::bind_rows()
    structure(list(coefs = coefs, stats = stats, name = name),
              class = c("sg_table", "stargate"))
}


#' @export
sg_standard_stats <- function(type) {
    list("r.squared" = "R2",
      "adj.r.squared" = "Adj. R2",
      "sigma" = NULL,
      "statistic" = NULL,
      "p.value" = NULL,
      "df" = NULL,
      "logLik" = "log Lik.",
      "AIC" = NULL,
      "BIC" = NULL,
      "deviance" = NULL,
      "df.residual" = NULL,
      "nobs" = "No. of Observations")
}


# přejmenuje jméno koeficientu, statistiky nebo modelu
#' @export
sg_rename <- function(x, coefs = NULL, stats = NULL, name = NULL) {
    if (!(inherits(x, "sg_model") || inherits(x, "sg_table")))
        stop("x must be either sg_model or sg_table")
    if (!is.null(coefs))
        stop("coefs not implemnted")
    if (!is.null(stats)) {
        keep_stats <- !purrr::map_lgl(stats, is.null)
        stats <- stats[keep_stats]
        x$stats <- dplyr::filter(x$stats, (stat %in% names(stats)))
        for (s in seq_along(stats))
            x$stats$stat[x$stat$stat == names(stats)[s]] <- stats[[s]]
    }
    x
}


formatted <- function(x, nsmall = 2, ...) {
    format(round(x, nsmall), nsmall = nsmall, ...)
}


sg_format_coefs <- function(x,
                            format = "{estimate}{stars} ({std.error})",
                            nsmall = 2,
                            stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                            ...) {
    starred <- function(p.value) {
        stars <- sort(stars)
        width <- max(length(names(stars)))
        purrr::map(p.value, ~names(stars)[. <= stars][1]) |>
            stringr::str_replace_na(replacement = "") |>
            stringr::str_pad(width, side = "right")
    }

    x$coefs <- x$coefs |>
        dplyr::mutate(formated_coefs = glue::glue(format,
                                     estimate = formatted(estimate, nsmall = nsmall, ...),
                                     std.error = formatted(std.error, nsmall = nsmall, ...),
                                     statistic = formatted(statistic, nsmall = nsmall, ...),
                                     p.value = formatted(p.value, nsmall = nsmall, ...),
                                     stars = starred(p.value)),
               formated_coefs = as.character(formated_coefs))
    x
}


sg_format_stats <- function(x, nsmall = 2, ...) {
    x$stats <- x$stats |>
        dplyr::mutate(formatted_stats = purrr::map_chr(val,
                                                       ~dplyr::case_when(
                                                           is.integer(.) ~ formatted(., nsmall = 0, ...),
                                                           is.double(.) ~ formatted(., nsmall = nsmall, ...),
                                                           TRUE ~ as.character(.)
                                                       )))
    x
}


#' @export
sg_format <- function(tab, nsmall = 2, ...) {
    name <- dplyr::if_else(tab$name$name == "",
                           stringr::str_c("(", seq_along(tab$name$name), ")"),
                           tab$name$name
    )
    get_together <- function(tab, name) {
        tab |>
            dplyr::select(1, 2, ncol(tab)) |>
            dplyr::group_by(model_id) |>
            dplyr::group_split(.keep = FALSE) |>
            purrr::reduce(dplyr::full_join, by = names(tab)[2]) |>
            dplyr::mutate(across(-1,
                                 ~stringr::str_replace_na(., replacement = ""))) |>
            setNames(c("oo", name))
    }
    coefs <- tab |>
        sg_format_coefs(nsmall = nsmall, ...) |>
        purrr::pluck("coefs") |>
        get_together(name)
    stats <- tab |>
        sg_format_stats(nsmall = nsmall, ...) |>
        purrr::pluck("stats") |>
        get_together(name)
    # res <- list(coefs = coefs, stats = stats)
    # class(res) <- c("sg_formated_table", "stargate")
    # res
    dplyr::bind_rows(coefs, stats)
}
