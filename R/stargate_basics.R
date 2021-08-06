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


#' #' @export
#' sg_standard_stats <- function(type) {
#'     list("r.squared" = "R2",
#'       "adj.r.squared" = "Adj. R2",
#'       "sigma" = NULL,
#'       "statistic" = NULL,
#'       "p.value" = NULL,
#'       "df" = NULL,
#'       "logLik" = "log Lik.",
#'       "AIC" = NULL,
#'       "BIC" = NULL,
#'       "deviance" = NULL,
#'       "df.residual" = NULL,
#'       "nobs" = "No. of Observations")
#' }


#' #' Renames estimated parameters and statisticts
#' #'
#' #' @param x A sg model or sg table.
#' #' @param coefs The list of names to be changed.
#' #' @param stats The list of statistics to be changed.
#' #' @param name The list of model names to be changed.
#' #' @return The sg model or sg table with changed names.
#' #' @section Details:
#' #'
#' #' If you want to remove a parameter or a statistics, set it to NULL.
#' #'
#' #' @section Warnings:
#' #'
#' #' It cannot be run twice with the same table because it would rename statistics
#' #' and then remove them.
#' #'
#' #' The change of model parameters is not implemented yet.
#' #' @examples
#' #' # st <- sg_rename(st, stats = sg_standard_stats())
#' #' @export
#' sg_rename <- function(x, coefs = NULL, stats = NULL, name = NULL) {
#'     if (!(inherits(x, "sg_model") || inherits(x, "sg_table")))
#'         stop("x must be either sg_model or sg_table")
#'     if (!is.null(coefs))
#'         stop("coefs not implemnted")
#'     if (!is.null(stats)) {
#'         keep_stats <- !purrr::map_lgl(stats, is.null)
#'         stats <- stats[keep_stats]
#'         x$stats <- dplyr::filter(x$stats, (stat %in% names(stats)))
#'         for (s in seq_along(stats))
#'             x$stats$stat[x$stat$stat == names(stats)[s]] <- stats[[s]]
#'     }
#'     x
#' }




# rename stuff ------------------------------------------------------------

sg_rename <- function(x, coefs = NULL, stats = NULL) {

}


sg_rename_coefs <- function(x, coefs) {
    if (!(inherits(x, "sg_model") || inherits(x, "sg_table") ||
          inherits(x, "sg_formatted_table")))
        stop("x must be a stargate object")
    if (!((is.character(coefs) && length(coefs >= 1) && !is.null(names(coefs))) ||
          (is.data.frame(coefs) && ncol == 2 && nrow >= 1)))
        stop("coefs must be a non-empty named character vector or a non-empty",
             " data.frame with two columns")
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

sg_reorder <- function(x) {

}



# format table ------------------------------------------------------------


formatted <- function(x, nsmall = 2, ...) {
    format(round(x, nsmall), nsmall = nsmall, ...)
}


sg_format_coefs <- function(coefs,
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

    purrr::map(format,
               ~ dplyr::mutate(
                   coefs,
                   formatted_coefs = glue::glue(.x,
                                               estimate = formatted(estimate, nsmall = nsmall),
                                               std.error = formatted(std.error, nsmall = nsmall),
                                               statistic = formatted(statistic, nsmall = nsmall),
                                               p.value = formatted(p.value, nsmall = nsmall),
                                               stars = starred(p.value)),
                   formatted_coefs = as.character(formatted_coefs))
    ) |>
        dplyr::bind_rows(.id = "row_id") |>
        dplyr::select(model_id, term, row_id, dplyr::everything()) |>
        dplyr::arrange(model_id, term, row_id)
}


sg_format_stats <- function(stats, nsmall = 2, ...) {
    dplyr::mutate(stats,
                  formatted_stats = purrr::map_chr(val,
                                                   ~dplyr::case_when(
                                                       is.integer(.) ~ formatted(., nsmall = 0, ...),
                                                       is.double(.) ~ formatted(., nsmall = nsmall, ...),
                                                       TRUE ~ as.character(.)
                                                   )))
}


#' @export
# TODO: add formatting parameters instead of ...
sg_format <- function(tab,
                      format = "{estimate}{stars} ({std.error})",
                      nsmall = 2,
                      stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      ...) {
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
        sg_format_coefs(format = format,
                        nsmall = nsmall,
                        stars = stars,
                        ...) |>
        get_together(name, tabname = "term")
    stats <- tab$stats |>
        sg_format_stats(nsmall = nsmall, ...) |>
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
