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
    coefs <- tidy(m)
    stats <- glance(m) |>
        mutate(across(everything(), list)) |>
        pivot_longer(everything(), names_to = "stat", values_to = "val")
    name <- tibble(name = name)
    mod <- list(coefs = coefs, stats = stats, name = name)
    class(mod) <- c("sg_model", "stargate")
    mod
}


#' @export
sg_table <- function(...) {
    count_models_in_tables <- function(x) {
        count <- map_int(x,
                         ~ case_when(
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
                mutate(model_id = id) |>
                select(model_id, everything())
        tab}
    }
    mods <- list(...)
    mods <- map(mods, sg_model)
    first_model_id <- count_models_in_tables(mods)
    model_ids <- first_model_id + mods |>
        map_int(~class(.)[1] == "sg_model") |>
        cumsum()
    coefs <- map2(mods, model_ids, ~add_model_id(.x, "coefs", .y)) |>
        bind_rows()
    stats <- map2(mods, model_ids, ~add_model_id(.x, "stats", .y)) |>
        bind_rows()
    name <- map2(mods, model_ids, ~add_model_id(.x, "name", .y)) |>
        bind_rows()
    tab <- list(coefs = coefs, stats = stats, name = name)
    class(tab) <- c("sg_table", "stargate")
    tab
}


# přejmenuje jméno koeficientu, statistiky nebo modelu
#' @export
sg_rename <- function(x, coef, stat, name) {

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
        map(p.value, ~names(stars)[. <= stars][1]) |>
            str_replace_na(replacement = "") |>
            str_pad(width, side = "right")
    }

    x$coefs <- x$coefs |>
        mutate(formated_coefs = glue(format,
                                     estimate = formatted(estimate, nsmall = nsmall, ...),
                                     std.error = formatted(std.error, nsmall = nsmall, ...),
                                     statistic = formatted(statistic, nsmall = nsmall, ...),
                                     p.value = formatted(p.value, nsmall = nsmall, ...),
                                     stars = starred(p.value)),
               formated_coefs = as.character(formated_coefs))
    x
}


sg_format_stats <- function(x, nsmall = 2, ...) {
    # pozor! formatted() je duplicitní s sg_format_coefs()
    x$stats <- x$stats |>
        mutate(formatted_stats = map_chr(val,
                                         ~case_when(
                                             is.integer(.) ~ formatted(., nsmall = 0, ...),
                                             is.double(.) ~ formatted(., nsmall = nsmall, ...),
                                             TRUE ~ as.character(.)
                                         )))
    x
}


#' @export
sg_format <- function(tab, nsmall = 2, ...) {
    name <- if_else(tab$name$name == "",
                    str_c("(", seq_along(tab$name$name), ")"),
                    tab$name$name
    )
    get_together <- function(tab, name) {
        tab |>
            select(1, 2, ncol(tab)) |>
            group_by(model_id) |>
            group_split(.keep = FALSE) |>
            reduce(full_join, by = names(tab)[2]) |>
            mutate(across(-1, ~str_replace_na(., replacement = ""))) |>
            setNames(c("oo", name))
    }
    coefs <- tab |>
        sg_format_coefs(nsmall = nsmall, ...) |>
        pluck("coefs") |>
        get_together(name)
    stats <- tab |>
        sg_format_stats(nsmall = nsmall, ...) |>
        pluck("stats") |>
        get_together(name)
    # res <- list(coefs = coefs, stats = stats)
    # class(res) <- c("sg_formated_table", "stargate")
    # res
    bind_rows(coefs, stats)
}
