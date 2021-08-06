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


# sg model ----------------------------------------------------------------

#' Convert an estimated model to a stargate model
#'
#' @param m A estimated model.
#' @param name The model's name in the final model table.
#' @param ... Other parameters sent to the \code{sg_model} methods.
#' @return The sg model representation of the estimate.
#' @examples
#' library(tibble)
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
sg_model.default <- function(m, name = "", ...) {
    if (inherits(m, "stargate"))
        return(m)
    warning("I'm using the default sg_model method for an estimate of class ",
            class(m),
            ". Some information may be 'lost in translation'.",
            "Check the details of broom::tidy() method for this class.")
    coefs <- broom::tidy(m)
    stats <- broom::glance(m) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), list)) |>
        tidyr::pivot_longer(dplyr::everything(),
                            names_to = "stat", values_to = "val")
    name <- tibble::tibble(name = name)
    structure(list(coefs = coefs, stats = stats, name = name),
              class = c("sg_model", "stargate"))
}



# sg table ----------------------------------------------------------------

split_model_table <- function(tab) {
    coefs <- tab$coefs |>
        dplyr::group_by(model_id) |>
        dplyr::group_split(.keep = FALSE)
    stats <- tab$stats |>
        dplyr::group_by(model_id) |>
        dplyr::group_split(.keep = FALSE)
    name <- tab$name |>
        dplyr::group_by(model_id) |>
        dplyr::group_split(.keep = FALSE)
    base::structure(purrr::pmap(list(coefs, stats, name),
                                function(coefs, stats, name)
                                    structure(list(coefs = coefs,
                                                   stats = stats,
                                                   name = name),
                                              class = c("sg_model", "stargate"))),
                    class = c("sg_model_list", "stargate")
    )

}

#' Join Several stargate Models into one stargate Table
#'
#' @param ... stargate models or model tables.
#'
#' @return A stargate model table.
#' @export
#'
#' @examples
#' n <- 1e3
#' df <- tibble(
#'     x1 = rnorm(n),
#'     x2 = rnorm(n),
#'     x3 = rnorm(n),
#'     e  = rnorm(n),
#'     y  = x1 - 2 * x2 + 3 * x3 + e
#' )
#'
#' m1 <- lm(y ~ x1 + x2, df)
#' m2 <- lm(y ~ x1 + x2 + x3, df)
#' m3 <- lm(y ~ x1 + x2 * x3, df)
#' sm1 <- sg_model(m1)
#' stab <- sg_table(sm1, m2, m3)
# sg_table <- function(...) {
#     count_models_in_tables <- function(x) {
#         count <- purrr::map_int(x,
#                                 ~ dplyr::case_when(
#                                     inherits(., "sg_model") ~ 0L,
#                                     inherits(., "sg_table") ~ length(unique(.$name$model_id)),
#                                     TRUE ~ -1L
#                                 ))
#         if (any(count == -1L)) stop("OOOOOoooo!")
#         sum(count)
#     }
#     add_model_id <- function(x, var, id) {
#         {tab <- x[[var]]
#         if (inherits(x, "sg_model"))
#             tab <- tab |>
#                 dplyr::mutate(model_id = id) |>
#                 dplyr::select(model_id, dplyr::everything())
#         tab}
#     }
#     mods <- list(...)
#     mods <- purrr::map(mods, sg_model)
#     first_model_id <- count_models_in_tables(mods)
#     model_ids <- first_model_id + mods |>
#         purrr::map_int(~class(.)[1] == "sg_model") |>
#         cumsum()
#     coefs <- purrr::map2(mods, model_ids, ~add_model_id(.x, "coefs", .y)) |>
#         dplyr::bind_rows()
#     stats <- purrr::map2(mods, model_ids, ~add_model_id(.x, "stats", .y)) |>
#         dplyr::bind_rows()
#     name <- purrr::map2(mods, model_ids, ~add_model_id(.x, "name", .y)) |>
#         dplyr::bind_rows()
#     structure(list(coefs = coefs, stats = stats, name = name),
#               class = c("sg_table", "stargate"))
# }
sg_table <- function(...) {
    mods <- list(...) |>
        purrr::map(sg_model) |>
        purrr::map_if(~inherits(., "sg_table"),
                      split_model_table)
    models <- list()
    for (k in seq_along(mods))
        if (inherits(mods[[k]], "sg_model_list")) {
            for (l in seq_along(mods[[k]]))
                models[[length(models) + 1]] <- mods[[k]][[l]]
        } else {
            models[[length(models) + 1]] <- mods[[k]]
        }
    coefs <- purrr::map(models, "coefs") |> dplyr::bind_rows(.id = "model_id")
    stats <- purrr::map(models, "stats") |> dplyr::bind_rows(.id = "model_id")
    name <- purrr::map(models, "name") |> dplyr::bind_rows(.id = "model_id")
    structure(list(coefs = coefs, stats = stats, name = name),
              class = c("sg_table", "stargate"))
}


#' @describeIn sg_table Joins a stargate model and a stargate model, table, or
#'     an object convertible to stargame model.
#' @export
`+.sg_model` <- function(e1, e2) {
    sg_table(e1, e2)
}


#' @describeIn sg_table Joins a stargate table and a stargate model, table, or
#'     an object convertible to stargame model.
#' @export
`+.sg_table` <- function(e1, e2) {
    sg_table(e1, e2)
}



# rename stuff ------------------------------------------------------------

#' Title Rename Coefficients or Statistics in a Stargate Model or Model Table
#'
#' @param x A stargate model or model table.
#' @param coefs A named character vector or a tibble with two columns: pattern
#'     and value.
#' @param stats A named character vector or a tibble with two columns: pattern
#'     and value.
#' @param regex A logical; \code{FALSE} by default.
#'
#' @section Details:
#'
#' If \code{regex} is \code{FALSE}, every coefficient or statistics which name
#' is equal to a pattern is completely replaced by a value. If \code{regex} is
#'  \code{TRUE}, stringr::str_replace_all() function is used.
#'
#' The individual items in \code{coefs} and \code{stats} are used sequentially
#' as rules which modify the results of the previous rules
#'
#' @return The same class as x.
#' @export
#'
#' @examples
#' n <- 1e3
#' df <- tibble(
#'     x1 = rnorm(n),
#'     x2 = rnorm(n),
#'     x3 = rnorm(n),
#'     e  = rnorm(n),
#'     y  = x1 - 2 * x2 + 3 * x3 + e
#' )
#'
#' m1 <- lm(y ~ x1 + x2, df)
#' m2 <- lm(y ~ x1 + x2 + x3, df)
#' m3 <- lm(y ~ x1 + x2 * x3, df)
#' stab <- sg_table(m1, m2, m3)
#'
#' stab |> sg_rename(coefs = c("a" = "x"))
#' stab |> sg_rename(coefs = c("a" = "x1"))
#' stab |> sg_rename(coefs = c("a" = "x", " x " = ":"), regex = TRUE)
sg_rename <- function(x, coefs = NULL, stats = NULL, regex = FALSE) {
    if (!(inherits(x, "sg_model") || inherits(x, "sg_table") ||
          inherits(x, "sg_formatted_table")))
        stop("x must be a stargate object")
    if (!((is.character(coefs) && length(coefs >= 1) && !is.null(names(coefs))) ||
          (is.data.frame(coefs) && ncol == 2 && nrow >= 1) || is.null(coefs)))
        stop("coefs must be a non-empty named character vector or a non-empty",
             " data.frame with two columns")
    if (!((is.character(stats) && length(stats >= 1) && !is.null(names(stats))) ||
          (is.data.frame(stats) && ncol == 2 && nrow >= 1) || is.null(stats)))
        stop("stats must be a non-empty named character vector or a non-empty",
             " data.frame with two columns")

    if (!is.null(coefs))
        x$coefs <- sg_rename_coefs_or_stats(x$coefs, rules = coefs,
                                            what = "term", regex = regex)
    if (!is.null(stats))
        x$stats <- sg_rename_coefs_or_stats(x$stats, rules = stats,
                                            what = "stat", regex = regex)
    x
}


sg_rename_coefs_or_stats <- function(x, rules, what, regex) {
    if (is.character(rules))
        rules <- tibble::tibble(pattern = as.character(rules),
                                value = names(rules))
    for (k in base::seq_len(base::nrow(rules)))
        if (regex) {
            x[[what]] <- stringr::str_replace_all(x[[what]],
                                                  rules$pattern[k],
                                                  rules$value[k])
        } else {
            x[[what]][x[[what]] == rules$pattern[k]] <- rules$value[k]
        }
    x
}



# remove stuff ------------------------------------------------------------

sg_remove_coefs_or_stats <- function(x, var, val, keep = FALSE, regex = FALSE) {
    if (!(inherits(x, "sg_model") || inherits(x, "sg_table") ||
          inherits(x, "sg_formatted_table")))
        stop("x must be a stargate object")
    if (is.null(val) || !((is.numeric(val) || is.character(val))
                          && length(val) >= 1))
        stop(var, " must be a non-empty character of numeric vector")
    term <- "bug"
    if (var == "coefs")
        term <- "term"
    if (var == "stats")
        term <- "stat"
    if (term == "bug")
        stop("var must be either coefs or stats")
    if (!(is.logical(keep) && length(keep == 1)))
        stop("keep must be either TRUE or FALSE")

    keep_num <- as.integer(keep) * 2 - 1
    keep_log <- identity
    if (!keep)
        keep_log <- Negate(keep_log)
    if (is.numeric(val))
        x[[var]] <- x[[var]][keep_num * val, ]
    if (is.character(val))
        if (regex) {
            for (cf in val)
                # x[[var]] <-
                #     dplyr::filter(x[[var]],
                #                   stringr::str_detect(dplyr::across({{ term }}),
                #                                       cf, negate = !keep))
            x[[var]] <- x[[var]][stringr::str_detect(x[[var]][[term]], cf, negate = !keep), ]
        } else {
            x[[var]] <- x[[var]][keep_log(x[[var]][[term]] %in% val), ]
        }
    x
}


#' Remove estimated parameters or model statistics
#'
#' These functions remove estimated parameters and/or model statistics from the
#' stargate models, tables, and formatted tables. The removed items may be
#' identified by position or a name.
#'
#' @param x A stargate model, table, or formatted table.
#' @param coefs The parameter estimates that should be removed. Either
#'     a non-empty numeric or character vector. If a numeric vector,
#'     the estimates on the given positions are removed. If a character vector,
#'     the estimates with given names are removed. See \code{regex} parameter.
#' @param stats The model statistics that should be removed. Either
#'     a non-empty numeric or character vector. If a numeric vector,
#'     the statistics on the given positions are removed. If a character vector,
#'     the statistics with given names are removed. See \code{regex} parameter.
#' @param keep A logical scalar. If \code{FALSE} (a default), the identified
#'     parameters and/or statistics are removed. If \code{TRUE}, only the
#'     identified parameters and/or statistics are kept.
#' @param regex A logical scalar. If \code{TRUE}, \code{coefs} and stats are
#'     regular expressions. If \code{FALSE} (a default), they are taken
#'     literally.
#' @return A stargate model, table, or formatted table.
#' @details If the removed items are identified with names and the \code{regex}
#'     is \code{TRUE}, they are used as successive rules. If \code{keep} is
#'     \code{FALSE}, the parameters that are matched by \code{coefs[1]} are
#'     removed first, then the parameters that are matched by \code{coefs[2]}
#'     and so on; the same for statistics.
#'     If \code{keep} is \code{FALSE}, the list of parameters and/or statistics
#'     is successively narrowed down.
#' @export
#'
#' @examples
#' library(tibble)
#' n <- 1e3
#' df <- tibble(
#'     x1 = rnorm(n),
#'     x2 = rnorm(n),
#'     x3 = rnorm(n),
#'     e  = rnorm(n),
#'     y  = x1 - 2 * x2 + 3 * x3 + e
#' )
#'
#' m1 <- lm(y ~ x1 + x2, df)
#' m2 <- lm(y ~ x1 + x2 + x3, df)
#' m3 <- lm(y ~ x1 + x2 * x3, df)
#' sm1 <- sg_model(m1)
#' stab <- sg_table(sm1, m2, m3)
#'
#' sg_remove_coefs(sm1, coefs = 1)
#' sg_remove_stats(sm1, stats = "nobs")
#' sg_remove(stab, coefs = "x1", stats = c("df", "nobs"))
#' sg_remove(stab, stats = c("r.squared", "logLik", "nobs"), keep = TRUE)
sg_remove <- function(x, coefs = NULL, stats = NULL,
                      keep = FALSE, regex = FALSE) {
    if (!(inherits(x, "sg_model") || inherits(x, "sg_table") || inherits(x, "sg_formatted_table")))
        stop("x must be a stargate object")
    if (!is.null(coefs))
        x <- sg_remove_coefs(x, coefs = coefs, keep = keep, regex = regex)
    if (!is.null(stats))
        x <- sg_remove_stats(x, stats = stats, keep = keep, regex = regex)
    x
}


#' @describeIn sg_remove Removes only parameter estimates.
#' @export
sg_remove_coefs <- function(x, coefs, keep = FALSE, regex = FALSE) {
    sg_remove_coefs_or_stats(x, var = "coefs", val = coefs,
                             keep = keep, regex = regex)
}


#' @describeIn sg_remove Removes only model statistics.
#' @export
sg_remove_stats <- function(x, stats = NULL, keep = FALSE, regex = FALSE) {
    sg_remove_coefs_or_stats(x, var = "stats", val = stats,
                             keep = keep, regex = regex)
}





# replace stuff -----------------------------------------------------------

sg_replace <- function(x) {

}



# reorder stuff -----------------------------------------------------------

sg_reorder <- function(x, coefs = NULL, stats = NULL) {

}

# sg_reorder_coefs_or_stats <- function(x, rules, what) {
#     if (is.numeric(rules)) {
#         rules <- base::unique(base::as.integer(rules))
#         rest <- base::setdiff(base::seq_len(base::nrow(x)), rules)
#         rules <- c(rules, rest)
#         return(x[rules, ])
#     }
# }



# format table ------------------------------------------------------------

formatted <- function(x, format) {
    if ("nsmall" %in% names(format))
        format$nsmall <- 2
    round_x <- round(x, format$nsmall)
    do.call("format", list(round_x, format = format))
}


sg_format_coefs <- function(coefs, style, format, stars) {
    starred <- function(p.value) {
        stars <- sort(stars)
        width <- max(length(names(stars)))
        purrr::map(p.value, ~names(stars)[. <= stars][1]) |>
            stringr::str_replace_na(replacement = "") |>
            stringr::str_pad(width, side = "right")
    }

    purrr::map(style,
               ~ dplyr::mutate(
                   coefs,
                   formatted_coefs = glue::glue(.x,
                                               estimate = formatted(estimate, format = format),
                                               std.error = formatted(std.error, format = format),
                                               statistic = formatted(statistic, format = format),
                                               p.value = formatted(p.value, format = format),
                                               stars = starred(p.value)),
                   formatted_coefs = as.character(formatted_coefs))
    ) |>
        dplyr::bind_rows(.id = "row_id") |>
        dplyr::select(model_id, term, row_id, dplyr::everything()) |>
        dplyr::arrange(model_id, term, row_id)
}


sg_format_stats <- function(stats, format) {
    dplyr::mutate(stats,
                  formatted_stats = purrr::map_chr(val,
                                                   ~dplyr::case_when(
                                                       is.integer(.) ~ formatted(., format = format),
                                                       is.double(.) ~ formatted(., format = format),
                                                       TRUE ~ as.character(.)
                                                   )))
}


#' Format a Model Table
#'
#' @param tab A stargate model table.
#' @param style A character vector that describes how to format coefficients.
#'     Its length determines the number of rows per each parameter.
#' @param format A named list of parameters to format() function. Its
#'     \code{nsmall} slot is used to round the number before it is formatted.
#' @param stars A named numeric vector of statistical significance. Its values
#'     set the thresholds for the significance coding, its names the
#'     corresponding codes.
#'
#' @return A stargate formatted table.
#' @export
#'
#' @examples
#' n <- 1e3
#' df <- tibble(
#'     x1 = rnorm(n),
#'     x2 = rnorm(n),
#'     x3 = rnorm(n),
#'     e  = rnorm(n),
#'     y  = x1 - 2 * x2 + 3 * x3 + e
#' )
#'
#' m1 <- lm(y ~ x1 + x2, df)
#' m2 <- lm(y ~ x1 + x2 + x3, df)
#' m3 <- lm(y ~ x1 + x2 * x3, df)
#' stab <- sg_table(m1, m2, m3)
#'
#' sg_table(sm1, m2, m3) |>
#' sg_remove(coefs = "Interc", regex = TRUE) |>
#'     sg_remove(stats = c("r.squared", "logLik", "nobs"), keep = TRUE) |>
#'     sg_format(style = c("{estimate}", "({std.error})"),
#'               format = list(nsmall = 3)) |>
#'     sg_present()
sg_format <- function(tab,
                      style = "{estimate}{stars} ({std.error})",
                      format = list(nsmall = 2),
                      stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1)) {
    name <- dplyr::if_else(tab$name$name == "",
                           stringr::str_c("(", seq_along(tab$name$name), ")"),
                           tab$name$name
    )
    get_together <- function(tab, name, tabname) {
        by_names <- tabname
        if (tabname == "term")
            by_names <- c(by_names, "row_id")
        tab |>
            dplyr::select(
                dplyr::any_of(c("model_id", "row_id", tabname,
                                "formatted_coefs", "formatted_stats"))
            ) |>
            dplyr::group_by(model_id) |>
            dplyr::group_split(.keep = FALSE) |>
            purrr::reduce(dplyr::full_join, by = by_names) |>
            dplyr::mutate(across(-base::seq_along(by_names),
                                 ~stringr::str_replace_na(., replacement = ""))) |>
            stats::setNames(c(base::rev(by_names), name))
    }
    coefs <- tab$coefs |>
        sg_format_coefs(style = style,
                        format = format,
                        stars = stars) |>
        get_together(name, tabname = "term")
    stats <- tab$stats |>
        sg_format_stats(format = format) |>
        get_together(name, tabname = "stat")
    structure(list(coefs = coefs, stats = stats),
              class = c("sg_formatted_table", "stargate"))
}



# present table -----------------------------------------------------------

#' @export
sg_present <- function(x) {
    if (!inherits(x, "sg_formatted_table"))
        stop("x must be a sg formatted table")
    coefs <- x$coefs |>
        dplyr::mutate(term = ifelse(row_id == 1, term, "")) |>
        dplyr::select(-row_id)
    stats <- x$stats |>
        dplyr::rename(term = stat)
    tab <- dplyr::bind_rows(coefs, stats) |>
        dplyr::select(term, dplyr::everything())
    names(tab) <- c("", names(tab)[-1])
    tab
}
