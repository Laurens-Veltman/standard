#' Create a Standard Curve From Known Data
#'
#' @param data A `data.frame` that contains the columns for concentration and
#'   observed response for the standard curve.
#' @param conc Name of the column that contains the concentration for the
#'   standard curve.
#' @param resp Name of the column that contains the response values for the
#'   standard curve.
#'
#' @return A linear model ( [lm()][stats::lm] ) object to be used as a standard curve, for use with
#'   `standard::std_curve_predict()` `broom::augment()` or `stats::predict()`.
#' @export
#'
#' @examples
std_curve_fit <- function(data, conc, resp) {

  # do lots of quasiquotation magic to make the formula work with any of the
  # user-supplied columns
  in_conc <- rlang::enquo(conc)
  in_resp <- rlang::enquo(resp)
  .f <- rlang::expr(!!dplyr::sym(rlang::quo_name(in_conc)) ~ !!dplyr::sym(rlang::quo_name(in_resp)))

  # fit the actual linear model with the data and the created forumla
  mod <- stats::lm(.f, data = data)

  # return the model
  class(mod) <- c(class(mod), "std_curve")

  mod
}


#' Use a Standard Curve to Calculate Unkown Values
#'
#' @param model A linear model, created with either `lm()` or
#'   `standard::std_curve_fit()`
#' @param unknowns A numeric vector of unknown values, which the standard curve
#'   will be used to predict their values.
#'
#' @return a [tibble][tibble::tibble-package] with a column for the unknown
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
#'   prot = prot
#' )
#'
#' unknowns <- c(0.554, 0.568, 0.705)
#'
#' data %>%
#'   standard::std_curve_fit(prot, abs) %>%
#'   standard::std_curve_predict(unknowns)
std_curve_predict <- function(model, unknowns, digits = 3) {
  stopifnot(is.vector(unknowns))

  variable_names <- colnames(model$model)

  unk <- tibble::tibble(
    !!variable_names[2] := unknowns
  )

  calculated_data <- purrr::quietly(broom::augment)(model, newdata = unk)$result %>%
    dplyr::select(
      !!variable_names[2],
      !!variable_names[1] := .data$.fitted
    )

  calculated_data[, 2] <- round(calculated_data[, 2],
                                digits = digits)

  output <- list(
    std_curve = model,
    calc_data = calculated_data
  )

  output <- structure(output, class = "std_prediction")

  output
}


#' Title
#'
#' @param x
#' @param ...
#' @export
#'
print.std_prediction <- function(x, ...) {
  print(x[["calc_data"]])
}

#' Title
#'
#' @param x
#' @param row.names
#' @param optional
#'
#' @return
#' @export
as.data.frame.std_prediction <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x[["calc_data"]], row.names = row.names, optional = optional, ...)
}


#' Title
#'
#' @param x
#' @param i
#' @param j
#'
#' @return
#' @export
`[.std_prediction` <- function(x, i, j) {
  x[i, j]
}

#' Plot a Standard Curve
#'
#' @param data Result of `std_curve_pred()` or `std_curve_fit()`.
#'
#' @return a [ggplot2][ggplot2::ggplot] plot with the standard curve and unkowns
#'   plotted, whch can be further customised using `ggplot` options.
#' @export
#' @importFrom rlang .data
#'
#' @examples
std_curve_plot <- function(data) {

  if (!methods::is(data, "std_prediction")) {
    stop("Input must be the output from std_curve_pred().")
  }

  r_squared <- summary(data[["std_curve"]])[["r.squared"]]
  raw_data <- data[["std_curve"]][["model"]]
  var_names <- colnames(raw_data)
  annotation_data <- data.frame(x = lerp_vec(raw_data[, 1], 0.01),
                                y = lerp_vec(raw_data[, 2], 0.8))

  raw_data %>%
    ggplot2::ggplot(
      ggplot2::aes_string(var_names[1], var_names[2])
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(
      colour = "gray40",
      method = "lm",
      formula = "y ~ x",
      se = FALSE
    ) +

    ggplot2::geom_point(
      data = data[["calc_data"]],
      shape = 4,
      size = 5
    ) +
    ggplot2::geom_segment(
      data = data[["calc_data"]],
      ggplot2::aes_string(
        x = var_names[1],
        xend = var_names[1],
        y = 0,
        yend = var_names[2]
      ),
      linetype = "dashed"
    ) +
    ggplot2::theme_classic() +
    ggtext::geom_richtext(
      data = annotation_data,
      mapping = ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = paste0(
          "R<sup>2</sup> = ",
          round(r_squared, 2),
          "<br>",
          std_paste_formula(data[["std_curve"]]),
          "<br>Calculated Unknowns:"
        )
      ),
      fill = NA,
      label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt"),
      vjust = -0.1,
      hjust = 0
    ) +
    ggpp::geom_table(
      data = annotation_data,
      label = list(data$calc_data),
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      hjust = 0,
      vjust = 1.1
    )

}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
plot.std_prediction <- function(x, ...) {
  standard::std_curve_plot(x)
}

#' Title
#'
#' @param model
#' @param digits
#'
#' @return
#' @export
#'
#' @examples
std_paste_formula <- function(model, digits = 3) {
  numbers <- stats::coef(model)

  axis_names <- colnames(model[["model"]])
  sign <- ifelse(numbers[["(Intercept)"]] < 0, "-", "+")

  paste(
    axis_names[1],
    "=",
    round(numbers[[axis_names[2]]], digits = digits),
    "*",
    axis_names[2],
    sign,
    round(abs(numbers[["(Intercept)"]]), digits = digits)
  )
}
