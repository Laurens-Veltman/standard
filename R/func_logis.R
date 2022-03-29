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
std_func_logis <- function(data, conc, resp) {


  in_conc <- dplyr::sym(rlang::quo_name(rlang::enquo(conc)))
  in_resp <- dplyr::sym(rlang::quo_name(rlang::enquo(resp)))
  # use quo_name, sym and expr to define the forumlar for the model from the
  # user supplied columns
  .f <- rlang::expr(!!in_resp ~ SSlogis(!!in_conc, Asym, xmid, scal))

  stats::nls(formula = .f, data = data)
}
