#' Title
#'
#' @param data
#' @param conc
#' @param resp
#'
#' @return
#' @export
#'
#' @examples
std_func_linear <- function(data, conc, resp) {
  # do lots of quasiquotation magic to make the formula work with any of the
  # user-supplied columns
  # enquoting the given columns, so they can be used in a function
  in_conc <- dplyr::sym(rlang::quo_name(rlang::enquo(conc)))
  in_resp <- dplyr::sym(rlang::quo_name(rlang::enquo(resp)))

  # construct the function from the user supplied columns
  .f <- rlang::expr(!!in_conc ~ !!in_resp)

  # fit the actual linear model with the data and the created forumla
  std_curve <- stats::lm(.f, data = data)

  # return the model
  class(std_curve) <- c("std_curve", "lm", "oldClass")

  std_curve
}
