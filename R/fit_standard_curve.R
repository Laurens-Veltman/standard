#' Create a Standard Curve From Known Data
#'
#' @param data A `data.frame` that contains the columns for concentration and observed response for the standard curve.
#' @param conc Name of the column that contains the concentration for the standard curve.
#' @param resp Name of the column that contains the response values for the standard curve.
#'
#' @return A linear model object to be used as a standard curve, for use with
#'   `standard::predict_from_curve()` `broom::augment()` or `stats::predict()`.
#' @export
#'
#' @examples
fit_standard_curve <- function(data, conc, resp) {

  # do lots of quasiquotation magic to make the formula work with any columns
  in_conc <- rlang::enquo(conc)
  in_resp <- rlang::enquo(resp)
  .f <- rlang::expr(!!dplyr::sym(rlang::quo_name(in_conc)) ~ !!dplyr::sym(rlang::quo_name(in_resp)))

  # fit the actual linear model with the data and the created forumla
  mod <- stats::lm(.f, data = data)

  # return the model
  mod
}


#' Use a Standard Curve to Calculate Unkown Values
#'
#' @param model A linear model, created with either `lm()` or
#'   `standard::fit_standard_curve()`
#' @param unkowns A numeric vector of unkown values, which the standard curve
#'   will be used to predict their values.
#'
#' @return a [tibble][tibble::tibble-package] with a column for the unkown
#'   values, and a column `.fitted` for the predicted values, based on the
#'   standard curve.
#' @export
#' @importFrom rlang .data :=
#'
#' @examples
#' prot <- c(
#'   0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000,
#'   0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000
#' )
#' abs <- c(
#'   0.329, 0.352, 0.349, 0.379, 0.417, 0.491, 0.668, 0.956,
#'   0.327, 0.341, 0.355, 0.383, 0.417, 0.446, 0.655, 0.905
#' )
#'
#' data <- tibble::tibble(
#'   abs = abs,
#'   prot = prot,
#'   somthing = seq_along(abs)
#' )
#'
#' unk <- c(0.554, 0.568, 0.705)
#'
#' standard::fit_standard_curve(data, prot, abs) %>%
#'   standard::predict_from_curve(unk)
predict_from_curve <- function(model, unkowns) {
  stopifnot(is.vector(unkowns))

  variable_names <- colnames(model$model)

  unk <- tibble::tibble(
    !!variable_names[2] := unkowns
  )

  calculated_data <- purrr::quietly(broom::augment)(model, newdata = unk)$result %>%
    dplyr::select(
      !!variable_names[2],
      !!variable_names[1] := .data$.fitted
    )

  calculated_data
}
