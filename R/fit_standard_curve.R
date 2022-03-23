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

  # setup the formula
  .f <- rlang::expr(!! dplyr::sym(rlang::quo_name(in_conc)) ~ !! dplyr::sym(rlang::quo_name(in_resp)))

  # fit the actual linear model
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
predict_from_curve <- function(model, unkowns) {
  stopifnot(is.vector(unkowns))

  measured_name <- names(stats::coef(model))[2]

  unk <- tibble::tibble(
    # rlang::`:=`(!!measured_name, unkowns)
    !!measured_name := unkowns
  )
  purrr::quietly(broom::augment)(model, newdata = unk)$result %>%
    dplyr::select(!!measured_name, .data$.fitted)
}






