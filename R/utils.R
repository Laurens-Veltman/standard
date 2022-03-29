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
