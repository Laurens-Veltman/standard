#' Finds Minimum Number of Decimal Places
#'
#' @param x Number to calculate decimal places.
#'
n_decimal <- function(x) {
  max_dec <- stringr::str_extract(as.character(x), "\\.\\d+$") %>%
    nchar() %>%
    min()

  max_dec - 1
}

#' Title
#'
#' @param x Value to lerp from.
#' @param y Value to lerp to.
#' @param z Proportion to lerp by (0-1).
lerp <- function(x, y, z = 0.5) {
  x + (y - x) * z
}


#' Lerp the Max and Min of a Vector.
#'
#' Finds the minium value of a vector and the maximum value of a vector, and
#' then lerps between the two by the factor `z`.
#'
#' @param vec numeric vector of values.
#' @param z proportion to lerp by (0-1).
lerp_vec <- function(vec, z = 0.5) {
  x <- min(vec)
  y <- max(vec)
  lerp(x, y, z)
}

#' Title
#'
#' @param mod
#' @param ...
#'
#' @return

quiet_broom_augment <- function(mod, ...) {
  purrr::quietly(broom::augment)(mod, ...)$result
}

#' Predicts Values for Line of Best Fit for Linear Model
#'
#' @param model `lm` from `std_curve_fit()`
#' @param values vector of concentration values to predict response for.
#'
#' @return a [tibble][tibble::tibble-package] with a column for concentrations
#'   and a column for predicted response
#'
#' @examples
linear_mod_predictor <- function(model, values = NULL) {

  model_data <- standard:::quiet_broom_augment(model)
  name_x <- colnames(model_data)[1]
  name_y <- colnames(model_data)[2]

  if (is.null(values)) {
    values <-
      seq(min(model_data[, name_x]), max(model_data[, name_x]), length.out = 100)
  }

  coefs <- coef(model)
  c <- coefs[1]
  m <- coefs[2]

  # rearrange the linear equation, to predict values of response from values of
  # concentration
  # y = mx + c, so (y - c) / m = x
  pred_values <- (values - c) / m

  df <- tibble::tibble(
    !!name_x := values,
    !!name_y := pred_values
  )

  df
}
