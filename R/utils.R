#' Finds Minimum Number of Decimal Places
#'
#' @param x Number to calculate decimal places.
#'
#' @return
n_decimal <- function(x) {
  max_dec <- stringr::str_extract(as.character(x), "\\.\\d+$") %>%
    nchar() %>%
    min()

  max_dec - 1
}

#' Title
#'
#' @param x
#' @param y
#' @param z
#'
#' @return
#' @export
#'
#' @examples
lerp <- function(x, y, z = 0.5) {
  x + (y - x) * z
}


#' Title
#'
#' @param vec
#' @param z
#'
#' @return
#' @export
#'
#' @examples
lerp_vec <- function(vec, z = 0.5) {
  x <- min(vec)
  y <- max(vec)
  lerp(x, y, z)
}
